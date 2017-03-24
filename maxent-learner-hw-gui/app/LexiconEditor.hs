{-# LANGUAGE LambdaCase, OverloadedStrings, ExtendedDefaultRules, TupleSections #-}

module LexiconEditor where

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
import Data.Maybe
import Text.PhonotacticLearner.PhonotacticConstraints
import Text.PhonotacticLearner.MaxentGrammar
import Text.PhonotacticLearner
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Array.IArray
import qualified Data.Map.Lazy as M
import qualified Data.ByteString as B
import qualified Data.Set as S
import System.IO
import Control.DeepSeq
import Text.Read (readMaybe)
import GtkUtils

data LexRow = LexRow {word :: [String], freq :: Int}

data LexSourceType = SpacedList | FieroList | FieroText

data LexSource = Custom | FromStored LexSourceType T.Text | Generated

segsFromFt :: FeatureTable String -> S.Set String
segsFromFt = M.keysSet . segLookup

parseWordlist :: [String] -> T.Text -> [LexRow]
parseWordlist seglist rawlist = do
    line <- T.lines rawlist
    let (rawword : rest) = T.split (== '\t') line
        word = segmentFiero seglist (T.unpack rawword)
        freq = fromMaybe 1 $ do
            [f] <- return rest
            readMaybe (T.unpack f)
    guard (word /= [])
    return $ LexRow word freq

collateWordlist :: [String] -> T.Text -> [LexRow]
collateWordlist seglist rawtext = fmap (uncurry LexRow) . M.assocs . M.fromListWith (+) $ do
    rawword <- T.words rawtext
    let word = segmentFiero seglist (T.unpack rawword)
    guard (word /= [])
    return (word, 1)

serWordlist :: [String] -> [LexRow] -> T.Text
serWordlist seglist = T.unlines . fmap (T.pack . showRow) where
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

watchLexModel :: LexSource -> ListStore LexRow -> Now (Behavior (LexSource, [LexRow]))
watchLexModel src model = do
    (lexChanged, changeLex) <- callbackStream
    initlist <- sync $ listStoreToList model
    let changecb = listStoreToList model >>= changeLex
    sync $ do
        on model rowChanged $ \_ _ -> changecb
        on model rowInserted $ \_ _ -> changecb
        on model rowDeleted $ \_ -> changecb
    sample $ fromChanges (src,initlist) (fmap (Custom,) lexChanged)



createEditableLexicon :: Behavior (S.Set String) -> EvStream [LexRow] -> Now (VBox, Behavior [LexRow])
createEditableLexicon currentsegs extreplace = do
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
    initDLex <- watchLexModel Custom initmodel

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

    return (vb, fmap snd currentLex)
