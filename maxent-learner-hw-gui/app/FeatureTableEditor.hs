{-# LANGUAGE LambdaCase #-}

module FeatureTableEditor where

import Graphics.UI.Gtk
import Control.FRPNow hiding (swap)
import Control.FRPNow.GTK
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Exception
import Data.Tuple
import Text.PhonotacticLearner.PhonotacticConstraints
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Array.IArray
import qualified Data.Map.Lazy as M
import qualified Data.ByteString as B
import System.IO
import Control.DeepSeq

data FTRow = FTRow T.Text (M.Map SegRef FeatureState) deriving (Eq, Show)

fsTrue FPlus = True
fsTrue FMinus = False
fsTrue FOff = False

fsInc FPlus = False
fsInc FMinus = False
fsInc FOff = True

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
    let segs = (segNames newft)
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

nothingOnIOError :: IOError -> IO (Maybe a)
nothingOnIOError _ = return Nothing

watchFtModel :: Array SegRef String -> ListStore FTRow -> Now (Behavior (FeatureTable String))
watchFtModel segs model = do
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
    vb <- sync $ vBoxNew False 0
    editor <- sync $ treeViewNew
    sync $ do
        scr <- scrolledWindowNew Nothing Nothing
        fr <- frameNew
        set fr [frameShadowType := ShadowIn ]
        containerAdd scr editor
        containerAdd fr scr
        boxPackStart vb fr PackGrow 0
    (ftReplaced, replacedft) <- callbackStream
    (initsegs, initmodel) <- sync $ setFTContents editor initft
    initdft <- watchFtModel initsegs initmodel
    dynft <- sample$ foldrSwitch initdft ftReplaced

    bar <- sync $ hBoxNew False 0

    csvfilter <- sync $ fileFilterNew
    allfilter <- sync $ fileFilterNew
    sync $ do
        fileFilterAddMimeType csvfilter "text/csv"
        fileFilterSetName csvfilter "CSV"
        fileFilterAddPattern allfilter "*"
        fileFilterSetName allfilter "All Files"

    loadButton <- sync $ buttonNewFromStock stockOpen
    loadPressed <- getUnitSignal buttonActivated loadButton
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
            Nothing -> do
                sync $ putStrLn "Invalid CSV table."
                return ()
            Just newft -> do
                (newsegs, newmodel) <- sync $ setFTContents editor newft
                newdft <- watchFtModel newsegs newmodel
                sync $ replacedft newdft
                sync $ putStrLn "Feature table sucessfully loaded."
        return ()

    saveButton <- sync $ buttonNewFromStock stockSaveAs
    savePressed <- getUnitSignal buttonActivated saveButton
    saveDialog <- sync $ fileChooserDialogNew (Just "Save Feature Table") transwin FileChooserActionSave
        [("gtk-cancel", ResponseCancel), ("gtk-save", ResponseAccept)]
    sync $ fileChooserAddFilter saveDialog csvfilter
    sync $ fileChooserAddFilter saveDialog allfilter
    flip callStream savePressed $ \_  -> do
        savePicked <- runFileChooserDialog saveDialog
        planNow . ffor savePicked $ \case
            Nothing -> return ()
            Just fn -> do
                ft <- sample dynft
                async $ do
                    let csv = featureTableToCsv id ft
                        bincsv = T.encodeUtf8 (T.pack csv)
                    B.writeFile fn bincsv
                    putStrLn $ "Wrote Feature Table " ++ fn
                return ()
        return ()

    sync $ do
        boxPackStart vb bar PackNatural 0
        boxPackStart bar loadButton PackNatural 0
        boxPackStart bar saveButton PackNatural 0

    return (vb, dynft)
