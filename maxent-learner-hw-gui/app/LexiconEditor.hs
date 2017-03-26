{-# LANGUAGE LambdaCase, OverloadedStrings, ExtendedDefaultRules, TupleSections #-}

module LexiconEditor (
    LexRow(..), createEditableLexicon, segsFromFt
) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.StyleContext
import Control.FRPNow hiding (swap, when)
import Control.FRPNow.GTK
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Exception
import Data.Foldable
import Data.Tuple
import Data.Tuple.Select
import Data.Maybe
import Text.PhonotacticLearner.PhonotacticConstraints
import Text.PhonotacticLearner.MaxentGrammar
import Text.PhonotacticLearner
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Data.Array.IArray
import qualified Data.Map.Lazy as M
import qualified Data.ByteString as B
import qualified Data.Set as S
import System.IO
import Control.DeepSeq
import Text.Read (readMaybe)
import GtkUtils

data LexRow = LexRow {word :: [String], freq :: Int}

data LexSourceType = FieroList | FieroText

data LexSource = Custom | FromStored LexSourceType T.Text

segsFromFt :: FeatureTable String -> S.Set String
segsFromFt = M.keysSet . segLookup

parseWordlist :: S.Set String -> T.Text -> [LexRow]
parseWordlist segs rawlist = do
    let seglist = toList segs
    line <- T.lines rawlist
    let (rawword : rest) = T.split (== '\t') line
        word = segmentFiero seglist (T.unpack rawword)
        freq = fromMaybe 1 $ do
            [f] <- return rest
            readMaybe (T.unpack f)
    guard (word /= [])
    return $ LexRow word freq

collateWordlist :: S.Set String -> T.Text -> [LexRow]
collateWordlist segs rawtext = fmap (uncurry LexRow) . M.assocs . M.fromListWith (+) $ do
    let seglist = toList segs
    rawword <- T.words rawtext
    let word = segmentFiero seglist (T.unpack rawword)
    guard (word /= [])
    return (word, 1)

serWordlist :: S.Set String -> [LexRow] -> T.Text
serWordlist segs = T.unlines . fmap (T.pack . showRow) where
    seglist = toList segs
    showRow (LexRow _ n) | n <= 0 = ""
    showRow (LexRow w 1) = joinFiero seglist w
    showRow (LexRow w n) = joinFiero seglist w ++ "\t" ++ show n


setLexContents :: TreeView -> S.Set String -> [LexRow] -> IO (ListStore LexRow)
setLexContents editor segs initlist = do
    let seglist = toList segs
    model <- listStoreNew initlist
    oldcols <- treeViewGetColumns editor
    forM_ oldcols $ \col -> treeViewRemoveColumn editor col
    treeViewSetModel editor model

    wcol <- treeViewColumnNew
    set wcol [treeViewColumnTitle := "Word", treeViewColumnExpand := True]
    wcell <- cellRendererTextNew
    set wcell [cellTextEditable := True]
    cellLayoutPackStart wcol wcell True
    treeViewAppendColumn editor wcol
    cellLayoutSetAttributes wcol wcell model $ \row -> [cellText := joinFiero seglist (word row)]
    on wcell edited $ \[i] rawword -> do
        let newword = segmentFiero seglist rawword
        row <- listStoreGetValue model i
        when (word row /= newword) $ listStoreSetValue model i (row {word = newword})

    adj <- adjustmentNew 1 1 10000 1 10 0
    fcol <- treeViewColumnNew
    set fcol [treeViewColumnTitle := "Frequency"]
    fcell <- cellRendererSpinNew
    set fcell [cellTextEditable := True, cellRendererSpinAdjustment := adj]
    cellLayoutPackStart fcol fcell True
    treeViewAppendColumn editor fcol
    cellLayoutSetAttributes fcol fcell model $ \row -> [cellText := show (freq row)]
    on fcell edited $ \[i] newval -> do
        row <- listStoreGetValue model i
        case readMaybe newval of
            Just newfreq | (freq row /= newfreq) -> listStoreSetValue model i (row {freq = newfreq})
            _ -> return ()

    return model

watchLexModel :: LexSource -> S.Set String -> ListStore LexRow -> Now (Behavior (LexSource, S.Set String, [LexRow]))
watchLexModel src segs model = do
    (lexChanged, changeLex) <- callbackStream
    initlist <- sync $ listStoreToList model
    let changecb = listStoreToList model >>= changeLex
    sync $ do
        on model rowChanged $ \_ _ -> changecb
        on model rowInserted $ \_ _ -> changecb
        on model rowDeleted $ \_ -> changecb
    sample $ fromChanges (src, segs, initlist) (fmap (Custom, segs, ) lexChanged)



createEditableLexicon :: Maybe Window -> Behavior (S.Set String) -> EvStream [LexRow] -> Now (VBox, Behavior [LexRow])
createEditableLexicon transwin currentsegs extreplace = do
    vb <- sync $ vBoxNew False 2
    editor <- sync treeViewNew
    sync $ do
        scr <- scrolledWindowNew Nothing Nothing
        scrolledWindowDisableOverlay scr
        fr <- frameNew
        set fr [frameShadowType := ShadowIn ]
        containerAdd scr editor
        containerAdd fr scr
        boxPackStart vb fr PackGrow 0
    bar <- sync $ hBoxNew False 2
    (addButton,addPressed) <- createIconButton "list-add" Nothing
    (delButton,delPressed) <- createIconButton "list-remove" Nothing
    (loadListButton, loadListPressed) <- createIconButton "document-open" (Just "Load Word List")
    (loadTextButton, loadTextPressed) <- createIconButton "document-open" (Just "Load Raw Text")
    (saveButton, savePressed) <- createIconButton "document-save" (Just "Save Word List")
    sync $ do
        boxPackStart vb bar PackNatural 0
        spacer <- hBoxNew False 0
        boxPackStart bar addButton PackNatural 0
        boxPackStart bar delButton PackNatural 0
        boxPackStart bar spacer PackGrow 10
        boxPackStart bar loadListButton PackNatural 0
        boxPackStart bar loadTextButton PackNatural 0
        boxPackStart bar saveButton PackNatural 0

    let segsChanged = toChanges currentsegs
    (modelChanged, changeModel) <- callbackStream
    (dLexChanged, changeDLex) <- callbackStream
    initsegs <- sample $ currentsegs
    initmodel <- sync $ setLexContents editor initsegs []
    initDLex <- watchLexModel Custom initsegs initmodel

    currentModel <- sample $ fromChanges initmodel modelChanged
    currentLex <- sample $ foldrSwitch initDLex dLexChanged

    flip callStream addPressed $ \_ -> do
        (store) <- sample currentModel
        let newRow = LexRow [] 1
        sync $ listStoreAppend store newRow
        return ()
    flip callStream delPressed $ \_ -> do
        (store) <- sample currentModel
        (cur, _) <- sync $ treeViewGetCursor editor
        sync $ case cur of
            [i] -> listStoreRemove store i
            _ -> return ()

    txtfilter <- sync fileFilterNew
    allfilter <- sync fileFilterNew
    sync $ do
        fileFilterAddMimeType txtfilter "text/*"
        fileFilterSetName txtfilter "Text Filter"
        fileFilterAddPattern allfilter "*"
        fileFilterSetName allfilter "All Files"

    saveDialog <- sync $ fileChooserDialogNew (Just "Save Lexicon") transwin FileChooserActionSave
        [("gtk-cancel", ResponseCancel), ("gtk-save", ResponseAccept)]
    sync $ fileChooserAddFilter saveDialog txtfilter
    sync $ fileChooserAddFilter saveDialog allfilter
    flip callStream savePressed $ \_  -> do
        (_,segs,rows) <- sample currentLex
        savePicked <- runFileChooserDialog saveDialog
        planNow . ffor savePicked $ \case
            Nothing -> return ()
            Just fn -> do
                async $ do
                    let out = serWordlist segs rows
                        binout = T.encodeUtf8 out
                    B.writeFile fn binout
                    putStrLn $ "Wrote Feature Table " ++ fn
                return ()
        return ()

    loadListDialog <- sync $ fileChooserDialogNew (Just "Load Lexicon") transwin FileChooserActionOpen
        [("gtk-cancel", ResponseCancel), ("gtk-open", ResponseAccept)]
    sync $ fileChooserAddFilter loadListDialog txtfilter
    sync $ fileChooserAddFilter loadListDialog allfilter
    flip callStream loadListPressed $ \_ -> do
        filePicked <- runFileChooserDialog loadListDialog
        loaded <- planNow . ffor filePicked $ \case
            Nothing -> return never
            Just fn -> async $ do
                rawfile <- fmap (T.decodeUtf8With T.lenientDecode) (B.readFile fn)
                evaluate rawfile
                return (fn,rawfile)
        planNow . ffor (join loaded) $ \(fn,rawfile) -> do
            segs <- sample currentsegs
            let initrows = parseWordlist segs rawfile
                src = FromStored FieroList rawfile
            newmodel <- sync $ setLexContents editor segs initrows
            newDLex <- watchLexModel src segs newmodel
            sync $ do
                changeModel newmodel
                changeDLex newDLex
                putStrLn "Lexicon sucessfully loaded."
        return ()

    loadTextDialog <- sync $ fileChooserDialogNew (Just "Load Text For New Lexicon") transwin FileChooserActionOpen
        [("gtk-cancel", ResponseCancel), ("gtk-open", ResponseAccept)]
    sync $ fileChooserAddFilter loadTextDialog allfilter
    sync $ fileChooserAddFilter loadTextDialog txtfilter
    flip callStream loadTextPressed $ \_ -> do
        filePicked <- runFileChooserDialog loadTextDialog
        loaded <- planNow . ffor filePicked $ \case
            Nothing -> return never
            Just fn -> async $ do
                rawfile <- fmap (T.decodeUtf8With T.lenientDecode) (B.readFile fn)
                evaluate rawfile
                return (fn,rawfile)
        planNow . ffor (join loaded) $ \(fn,rawfile) -> do
            segs <- sample currentsegs
            let initrows = collateWordlist segs rawfile
                src = FromStored FieroText rawfile
            newmodel <- sync $ setLexContents editor segs initrows
            newDLex <- watchLexModel src segs newmodel
            sync $ do
                changeModel newmodel
                changeDLex newDLex
                putStrLn "Lexicon sucessfully created."
        return ()

    flip callStream segsChanged $ \newsegs' -> do
        let newsegs = last newsegs'
        (src, oldsegs, oldrows) <- sample currentLex
        let newrows = case src of
                Custom -> let raw = serWordlist oldsegs oldrows
                          in parseWordlist newsegs raw
                FromStored FieroList raw -> parseWordlist newsegs raw
                FromStored FieroText raw -> collateWordlist newsegs raw
        newmodel <- sync $ setLexContents editor newsegs newrows
        newDLex <- watchLexModel src newsegs newmodel
        sync $ do
            changeModel newmodel
            changeDLex newDLex
            putStrLn "Lexicon resegmented."

    flip callStream extreplace $ \newrows' -> do
        let newrows = last newrows'
        segs <- sample currentsegs
        newmodel <- sync $ setLexContents editor segs newrows
        newDLex <- watchLexModel Custom segs newmodel
        sync $ do
            changeModel newmodel
            changeDLex newDLex

    return (vb, fmap sel3 currentLex)
