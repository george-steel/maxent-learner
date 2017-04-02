{-# LANGUAGE LambdaCase, OverloadedStrings, ExtendedDefaultRules #-}

module FeatureTableEditor (
    createEditableFT,
    displayFeatureMatrix,
    displayDynFeatureTable
) where

import Graphics.UI.Gtk
import Control.FRPNow hiding (swap)
import Control.FRPNow.GTK
import Control.Monad
import Control.Exception
import Data.Tuple
import Text.PhonotacticLearner.PhonotacticConstraints
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Array.IArray
import qualified Data.Map.Lazy as M
import qualified Data.ByteString as B
import Control.DeepSeq
import GtkUtils

default (T.Text)


data FTRow = FTRow T.Text (M.Map SegRef FeatureState) deriving (Eq, Show)

fsTrue FPlus = True
fsTrue FMinus = False
fsTrue FOff = False

fsInc FPlus = False
fsInc FMinus = True
fsInc FOff = False

fsCycle FPlus = FMinus
fsCycle FMinus = FOff
fsCycle FOff = FPlus

ft2rows ft = ffor (assocs . featNames $ ft) $ \(fi,fn) -> let
    sbounds = bounds (segNames ft)
    sidxs = indices (segNames ft)
    fsmap = M.fromList [(i,ftlook ft i fi) | i <- sidxs]
    in FTRow fn fsmap

rows2ft :: Array SegRef String -> [FTRow] -> FeatureTable String
rows2ft segs rows = FeatureTable ftarr fnames segs flook slook where
    nf = length rows
    (sa,sb) = bounds segs
    nrows = zip [1..] rows
    fnames = array (1,nf) [(n,fn) | (n,FTRow fn _) <- nrows]
    ftarr = array ((sa,1),(sb,nf)) $ do
        (fi, FTRow _ fsmap) <- nrows
        (si,fs) <- M.assocs fsmap
        return ((si,fi),fs)
    slook = M.fromList (fmap swap (assocs segs))
    flook = M.fromList (fmap swap (assocs fnames))


setFTContents :: TreeView -> FeatureTable String -> IO (Array SegRef String, ListStore FTRow)
setFTContents editor newft = do
    let segs = segNames newft
        rows = ft2rows newft
    model <- listStoreNew rows
    oldcols <- treeViewGetColumns editor
    forM_ oldcols $ \col -> treeViewRemoveColumn editor col
    treeViewSetModel editor model

    lcol <- treeViewColumnNew
    set lcol [treeViewColumnTitle := "Feature"]
    lcell <- cellRendererTextNew
    set lcell [cellTextEditable := True]
    cellLayoutPackStart lcol lcell True
    treeViewAppendColumn editor lcol
    cellLayoutSetAttributes lcol lcell model $ \(FTRow fn _) ->
        [cellText := fn]
    on lcell edited $ \[i] newtext -> do
        FTRow _ fsmap <- listStoreGetValue model i
        listStoreSetValue model i (FTRow newtext fsmap)

    forM_ (assocs segs) $ \(si,sn) -> do
        col <- treeViewColumnNew
        set col [treeViewColumnTitle := sn]
        cell <- cellRendererToggleNew
        cellLayoutPackStart col cell True
        treeViewAppendColumn editor col
        cellLayoutSetAttributes col cell model $ \(FTRow _ fsmap) ->
            [cellToggleActive := fsTrue (fsmap M.! si),
            cellToggleInconsistent := fsInc (fsmap M.! si)]
        on cell cellToggled $ \tpStr -> do
            let [i] = stringToTreePath tpStr
            FTRow fn fsmap <- listStoreGetValue model i
            let newrow = FTRow fn (M.adjust fsCycle si fsmap)
            listStoreSetValue model i newrow
    pcol <- treeViewColumnNew
    treeViewAppendColumn editor pcol
    return (segs, model)


watchFtModel :: (Array SegRef String, ListStore FTRow) -> Now (Behavior (FeatureTable String))
watchFtModel (segs, model) = do
    (rowsChanged, rowcb) <- callbackStream
    let changecb = listStoreToList model >>= rowcb
    sync $ do
        on model rowChanged $ \_ _ -> changecb
        on model rowInserted $ \_ _ -> changecb
        on model rowDeleted $ \_ -> changecb
    initrows <- sync $ listStoreToList model
    dynrows <- sample $ fromChanges initrows rowsChanged
    return $ fmap (rows2ft segs) dynrows

loadFTfromFile :: FilePath -> IO (Maybe (FeatureTable String))
loadFTfromFile fp = handle nothingOnIOError $ do
    bincsv <- B.readFile fp
    evaluate $ force . csvToFeatureTable id . T.unpack =<< either (const Nothing) Just (T.decodeUtf8' bincsv)

createEditableFT :: Maybe Window -> FeatureTable String -> Now (VBox, Behavior (FeatureTable String))
createEditableFT transwin initft = do
    vb <- sync $ vBoxNew False 2
    top <- sync stackNew
    editor <- sync treeViewNew

    (loadButton, loadPressed) <- createButton (Just "document-open") (Just "Load Table")
    (saveButton, savePressed) <- createButton (Just "document-save") (Just "Save Table")
    (addButton, addPressed) <- createButton (Just "list-add") Nothing
    (delButton, delPressed) <- createButton (Just "list-remove") Nothing
    (editButton, isEditing) <- createToggleButton (Just "accessories-text-editor") (Just "Edit Table") False

    (ftReplaced, replaceft) <- callbackStream
    (modelReplaced, replaceModel) <- callbackStream
    initmodel <- sync $ setFTContents editor initft
    currentModel <- sample $ fromChanges initmodel modelReplaced

    initdft <- watchFtModel initmodel
    callStream (sync . replaceft <=< watchFtModel . last) modelReplaced
    currentft <- sample$ foldrSwitch initdft ftReplaced

    viewer <- displayDynFeatureTable currentft

    sync $ do
        bar <- hBoxNew False 2
        spacer <- hBoxNew False 0
        boxPackStart bar addButton PackNatural 0
        boxPackStart bar delButton PackNatural 0
        boxPackStart bar spacer PackGrow 10
        boxPackStart bar editButton PackNatural 0
        boxPackStart bar loadButton PackNatural 0
        boxPackStart bar saveButton PackNatural 0

        scr <- scrolledWindowNew Nothing Nothing
        scrolledWindowDisableOverlay scr
        fr <- frameNew
        set fr [frameShadowType := ShadowIn ]
        containerAdd scr editor

        stackAddNamed top viewer "False"
        stackAddNamed top scr "True"
        boxPackStart vb top PackGrow 0
        boxPackStart vb bar PackNatural 0

    setAttr stackVisibleChildName top (fmap show isEditing)
    setAttr widgetSensitive addButton isEditing
    setAttr widgetSensitive delButton isEditing

    csvfilter <- sync fileFilterNew
    allfilter <- sync fileFilterNew
    sync $ do
        fileFilterAddMimeType csvfilter "text/csv"
        fileFilterSetName csvfilter "CSV"
        fileFilterAddPattern allfilter "*"
        fileFilterSetName allfilter "All Files"

    loadDialog <- sync $ fileChooserDialogNew (Just "Open Feature Table") transwin FileChooserActionOpen
        [("gtk-cancel", ResponseCancel), ("gtk-open", ResponseAccept)]
    sync $ fileChooserAddFilter loadDialog csvfilter
    sync $ fileChooserAddFilter loadDialog allfilter
    flip callStream loadPressed $ \_ -> do
        filePicked <- runFileChooserDialog loadDialog
        emft <- planNow . ffor filePicked $ \case
            Nothing -> return never
            Just fn -> async $ loadFTfromFile fn
        planNow . ffor (join emft) $ \case
            Nothing -> sync $ putStrLn "Invalid CSV table."
            Just newft -> sync $ do
                newmodel <- setFTContents editor newft
                replaceModel newmodel
                putStrLn "Feature table sucessfully loaded."
        return ()

    saveDialog <- sync $ fileChooserDialogNew (Just "Save Feature Table") transwin FileChooserActionSave
        [("gtk-cancel", ResponseCancel), ("gtk-save", ResponseAccept)]
    sync $ fileChooserAddFilter saveDialog csvfilter
    sync $ fileChooserAddFilter saveDialog allfilter
    flip callStream savePressed $ \_  -> do
        savePicked <- runFileChooserDialog saveDialog
        planNow . ffor savePicked $ \case
            Nothing -> return ()
            Just fn -> do
                ft <- sample currentft
                async $ do
                    let csv = featureTableToCsv id ft
                        bincsv = T.encodeUtf8 (T.pack csv)
                    B.writeFile fn bincsv
                    putStrLn $ "Wrote Feature Table " ++ fn
                return ()
        return ()

    flip callStream addPressed $ \_ -> do
        (segs, store) <- sample currentModel
        let newRow = FTRow "" (M.fromList [(s,FOff) | s <- indices segs])
        sync $ listStoreAppend store newRow
        return ()
    flip callStream delPressed $ \_ -> do
        (segs, store) <- sample currentModel
        (cur, _) <- sync $ treeViewGetCursor editor
        sync $ case cur of
            [i] -> listStoreRemove store i
            _ -> return ()

    return (vb, currentft)

displayFeatureMatrix :: FeatureTable String -> IO Grid
displayFeatureMatrix ft = do
    g <- gridNew
    set g [widgetName := Just "featuretable"]
    --set g [ containerBorderWidth := 5 ]
    --gridSetColumnSpacing g 2
    forM_ (assocs (segNames ft)) $ \(Seg n,s) -> do
        l <- labelNew (Just s)
        let oddclass = if odd n then ["oddcol"] else []
        widgetAddClasses l $ ["segheader"] ++ oddclass
        gridAttach g l n 0 1 1
    forM_ (assocs (featNames ft)) $ \(n,f) -> do
        l <- labelNew (Just f)
        widgetAddClasses l ["featheader"]
        set l [miscXalign := 0]
        gridAttach g l 0 n 1 1
    forM_ (assocs (featTable ft)) $ \((Seg s, f), fs) -> do
        l <- case fs of
            FPlus -> labelNew (Just "+")
            FMinus -> labelNew (Just "−")
            FOff -> do
                l <- labelNew (Just "0")
                widgetAddClasses l ["featzero"]
                return l
        let oddclass = if odd s then ["oddcol"] else []
        widgetAddClasses l oddclass
        gridAttach g l s f 1 1
    return g

displayDynFeatureTable :: Behavior (FeatureTable String) -> Now ScrolledWindow
displayDynFeatureTable dynft = do
    initft <- sample dynft
    let ftchanged = toChanges dynft
    scr <- sync $ scrolledWindowNew Nothing Nothing
    initwidget <- sync $ displayFeatureMatrix initft
    sync $ scrolledWindowAddWithViewport scr initwidget
    Just vp' <- sync $ binGetChild scr
    let vp = castToViewport vp'
    flip callIOStream ftchanged $ \newft -> do
        Just oldw <- binGetChild vp
        widgetDestroy oldw
        newwidget <- displayFeatureMatrix newft
        containerAdd vp newwidget
        widgetShowAll newwidget
    return scr
