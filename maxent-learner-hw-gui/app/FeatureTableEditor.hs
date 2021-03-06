{-# LANGUAGE LambdaCase, OverloadedStrings, ExtendedDefaultRules #-}

module FeatureTableEditor (
    createEditableFT,
    displayFeatureMatrix,
    displayDynFeatureTable
) where

import Graphics.UI.Gtk
import Control.FRPNow hiding (swap)
import Control.FRPNow.GTK
import Control.FRPNow.GTK.MissingFFI
import Control.Monad
import Control.Exception
import Data.Tuple
import Data.List
import Data.Maybe
import Text.PhonotacticLearner.PhonotacticConstraints
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Array.IArray
import qualified Data.Map.Lazy as M
import qualified Data.ByteString as B
import Control.DeepSeq

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

resegft :: [String] -> FeatureTable String -> FeatureTable String
resegft segs oldft = FeatureTable ftarr fnames segsarr flook slook where
    fnames = featNames oldft
    flook = featLookup oldft
    (fa,fb) = bounds fnames
    uniqsegs = nub segs
    nsegs = length uniqsegs
    segsarr = listArray (Seg 1, Seg nsegs) uniqsegs
    slook = M.fromList (fmap swap (assocs segsarr))
    ftarr = array ((Seg 1,fa), (Seg nsegs,fb)) $ do
        f <- indices fnames
        (sr,s) <- assocs segsarr
        let fs = fromMaybe FOff $ do
                sr' <- M.lookup s (segLookup oldft)
                return $ ftlook oldft sr' f
        return ((sr,f),fs)

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
loadFTfromFile fp = fmap join . checkIOError $ do
    bincsv <- B.readFile fp
    evaluate $ force . csvToFeatureTable id . T.unpack =<< either (const Nothing) Just (T.decodeUtf8' bincsv)

runTextDialog :: Maybe Window -> T.Text -> T.Text -> Now (Event (Maybe T.Text))
runTextDialog transwin q defa = do
    (retev, cb) <- callback
    dia <- sync $ dialogNew
    ent <- sync $ entryNew
    sync $ do
        case transwin of
            Just win -> set dia [windowTransientFor := win, windowModal := True]
            Nothing -> return ()
        dialogAddButton dia "gtk-ok" ResponseOk
        dialogAddButton dia "gtk-cancel" ResponseCancel
        ca <- castToBox <$> dialogGetContentArea dia
        entrySetText ent defa
        lbl <- createLabel q
        boxPackStart ca lbl PackGrow 0
        boxPackStart ca ent PackNatural 0
        on dia response $ \resp -> do
            case resp of
                ResponseOk -> do
                    txt <- entryGetText ent
                    cb (Just txt)
                _ -> cb Nothing
            widgetDestroy dia
        widgetShowAll dia
    return retev

createEditableFT :: Maybe Window -> FeatureTable String -> Now (VBox, Behavior (FeatureTable String))
createEditableFT transwin initft = do
    editor <- sync treeViewNew
    editor' <- createScrolledWindow editor
    sync $ set editor' [scrolledWindowOverlay := False]

    (loadButton, loadPressed) <- createButton (Just "document-open") (Just "Load Table")
    (saveButton, savePressed) <- createButton (Just "document-save") (Just "Save Table")
    (addButton, addPressed) <- createButton (Just "list-add") Nothing
    (delButton, delPressed) <- createButton (Just "list-remove") Nothing
    (editButton, isEditing) <- createToggleButton (Just "accessories-text-editor") (Just "Edit Table") False
    (segButton, segPressed) <- createButton Nothing (Just "Change Segments")
    setAttr widgetSensitive addButton isEditing
    setAttr widgetSensitive delButton isEditing

    (ftReplaced, replaceft) <- callbackStream
    (modelReplaced, replaceModel) <- callbackStream
    initmodel <- sync $ setFTContents editor initft
    currentModel <- sample $ fromChanges initmodel modelReplaced

    initdft <- watchFtModel initmodel
    callStream (sync . replaceft <=< watchFtModel . last) modelReplaced
    currentft <- sample$ foldrSwitch initdft ftReplaced

    viewer <- displayDynFeatureTable currentft

    top <- sync stackNew
    sync $ do
        stackAddNamed top viewer "False"
        stackAddNamed top editor' "True"
    setAttr stackVisibleChildName top (fmap show isEditing)

    vb <- createVBox 2 $ do
        bstretch =<< createFrame ShadowIn top
        bpack <=< createHBox 2 $ do
            bpack addButton
            bpack delButton
            bspacer
            bpack segButton
            bpack editButton
            bpack loadButton
            bpack saveButton

    {-csvfilter <- sync fileFilterNew
    allfilter <- sync fileFilterNew
    sync $ do
        fileFilterAddMimeType csvfilter "text/csv"
        fileFilterSetName csvfilter "CSV"
        fileFilterAddPattern allfilter "*"
        fileFilterSetName allfilter "All Files"
    -}

    loadDialog <- sync $ fileChooserDialogNew (Just "Open Feature Table") transwin FileChooserActionOpen
        [("gtk-cancel", ResponseCancel), ("gtk-open", ResponseAccept)]
    --sync $ fileChooserAddFilter loadDialog csvfilter
    --sync $ fileChooserAddFilter loadDialog allfilter
    sync $ set loadDialog [windowModal := True]
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
    --sync $ fileChooserAddFilter saveDialog csvfilter
    --sync $ fileChooserAddFilter saveDialog allfilter
    sync $ set saveDialog [windowModal := True]
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

    flip callStream segPressed $ \_ -> do
        oldft <- sample currentft
        let oldsegs = T.pack . unwords . elems . segNames $ oldft
        enewsegs <- runTextDialog transwin "Enter a new set of segments." oldsegs
        planNow . ffor enewsegs $ \case
            Nothing -> return ()
            Just newsegs -> let segs = words (T.unpack newsegs) in case segs of
                [] -> return ()
                _ -> sync $ do
                    let newft = resegft segs oldft
                    newmodel <- setFTContents editor newft
                    replaceModel newmodel
                    putStrLn "Segments Changed."
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
        widgetAddClasses (["segheader"] ++ oddclass) l
        gridAttach g l n 0 1 1
    forM_ (assocs (featNames ft)) $ \(n,f) -> do
        l <- labelNew (Just f)
        widgetAddClasses ["featheader"] l
        set l [miscXalign := 0]
        gridAttach g l 0 n 1 1
    forM_ (assocs (featTable ft)) $ \((Seg s, f), fs) -> do
        l <- case fs of
            FPlus -> labelNew (Just "+")
            FMinus -> labelNew (Just "−")
            FOff -> do
                l <- labelNew (Just "0")
                widgetAddClasses ["featzero"] l
                return l
        let oddclass = if odd s then ["oddcol"] else []
        widgetAddClasses oddclass l
        gridAttach g l s f 1 1
    return g

displayDynFeatureTable :: Behavior (FeatureTable String) -> Now ScrolledWindow
displayDynFeatureTable dynft = do
    initft <- sample dynft
    scr <- sync $ scrolledWindowNew Nothing Nothing
    done <- getUnrealize scr
    let ftchanged = toChanges dynft `beforeEs` done
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
