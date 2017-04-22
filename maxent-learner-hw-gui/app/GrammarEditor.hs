{-# LANGUAGE LambdaCase, OverloadedStrings, ExtendedDefaultRules, TupleSections #-}

module GrammarEditor where

import Graphics.UI.Gtk
import Control.FRPNow hiding (swap, when)
import Control.FRPNow.GTK
import Control.Monad
import Control.Exception
import Data.Foldable
import Data.Maybe
import Text.PhonotacticLearner.PhonotacticConstraints
import Text.PhonotacticLearner.PhonotacticConstraints.FileFormats
import Text.PhonotacticLearner.MaxentGrammar
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Map.Lazy as M
import qualified Data.ByteString as B
import qualified Data.Set as S
import Data.Array.IArray
import Control.DeepSeq
import Text.Read (readMaybe)

default (T.Text)


createLoadableGrammar :: Maybe Window -> Behavior (S.Set T.Text) -> EvStream PhonoGrammar -> Now (VBox, Behavior (Maybe PhonoGrammar))
createLoadableGrammar transwin validsegs extreplace = do

    (grammarLoaded, loadReplace) <- callbackStream
    let grammarChanged = merge grammarLoaded (fmap Just extreplace)
    currentGrammar <- sample $ fromChanges Nothing grammarChanged

    let lendesc = fmap (maybe "" (T.pack . show . assocs . lengthDist)) currentGrammar
        ruledesc = fmap (fromMaybe "" . fmap (\(PhonoGrammar _ g w) -> serGrammarRules g w)) currentGrammar
        gvalid = do
            segs <- validsegs
            let allValid (ClassGlob _ _ gp) = all (\(_,NClass _ fs) -> all (\(_,f) -> S.member f segs) fs) gp
            cg <- currentGrammar
            return $ fmap (\(PhonoGrammar _ g _) -> all allValid g) cg
        validdesc = ffor gvalid $ \case
            Just True -> ""
            Just False -> "Invalid Features Detected, will ignore"
            Nothing -> "No Grammar Loaded"

    lenlabel <- createLabelDisplay lendesc
    validlabel <- createLabelDisplay validdesc
    tv <- createTextViewDisplay ruledesc

    (loadButton, loadPressed) <- createButton (Just "document-open") (Just "Load Grammar")
    (saveButton, savePressed) <- createButton (Just "document-save") (Just "Save Grammar")
    setAttr widgetSensitive saveButton (fmap isJust currentGrammar)

    vb <- createVBox 2 $ do
        bpack lenlabel
        bstretch =<< createScrolledWindow tv
        bpack <=< createHBox 2 $ do
            bpack validlabel
            bspacer
            bpack loadButton
            bpack saveButton

    txtfilter <- sync fileFilterNew
    allfilter <- sync fileFilterNew

    sync $ do
        fileFilterAddMimeType txtfilter "text/*"
        fileFilterSetName txtfilter "Text Files"
        fileFilterAddPattern allfilter "*"
        fileFilterSetName allfilter "All Files"

    loadDialog <- sync $ fileChooserDialogNew (Just "Load Grammar") transwin FileChooserActionOpen
        [("gtk-cancel", ResponseCancel), ("gtk-open", ResponseAccept)]
    sync $ fileChooserAddFilter loadDialog txtfilter
    sync $ fileChooserAddFilter loadDialog allfilter
    flip callStream loadPressed $ \_ -> do
        filePicked <- runFileChooserDialog loadDialog
        planNow . ffor filePicked $ \case
            Nothing -> return never
            Just fn -> async $ do
                rawfile <- fmap (T.decodeUtf8With T.lenientDecode) (B.readFile fn)
                mg <- evaluate . force $ parseGrammar rawfile
                loadReplace mg
                putStrLn $ case mg of
                    Just _ -> "Grammar loaded: " ++ fn
                    Nothing -> "Invalid Grammar: " ++ fn
        return ()

    saveDialog <- sync $ fileChooserDialogNew (Just "Save Grammar") transwin FileChooserActionSave
        [("gtk-cancel", ResponseCancel), ("gtk-save", ResponseAccept)]
    sync $ fileChooserAddFilter saveDialog txtfilter
    sync $ fileChooserAddFilter saveDialog allfilter
    flip callStream savePressed $ \_  -> do
        mg <- sample currentGrammar
        case mg of
            Just g -> do
                savePicked <- runFileChooserDialog saveDialog
                planNow . ffor savePicked $ \case
                    Nothing -> return ()
                    Just fn -> do
                        async $ do
                            let out = serGrammar g
                                binout = T.encodeUtf8 out
                            B.writeFile fn binout
                            putStrLn $ "Wrote Grammar " ++ fn
                        return ()
                return ()
            Nothing -> return ()

    return (vb, currentGrammar)
