{-# LANGUAGE LambdaCase, OverloadedStrings, ExtendedDefaultRules, TupleSections, ScopedTypeVariables #-}

module GrammarEditor where

import Graphics.UI.Gtk
import Control.FRPNow hiding (swap, when)
import Control.FRPNow.GTK
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Data.Foldable
import Data.Maybe
import Text.PhonotacticLearner.PhonotacticConstraints
import Text.PhonotacticLearner.PhonotacticConstraints.FileFormats
import Text.PhonotacticLearner.MaxentGrammar
import Text.PhonotacticLearner.Util.Ring
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Map.Lazy as M
import qualified Data.ByteString as B
import qualified Data.Set as S
import Data.Array.IArray
import Control.DeepSeq
import Text.Read (readMaybe)
import Data.IORef
import Numeric

import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Chart.Easy hiding (indices, set')
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.State(EC, execEC)

default (T.Text)

createChartWidget :: (Eq a, ToRenderable c) => Behavior a -> (a -> c) -> Now DrawingArea
createChartWidget datafeed chartmaker = do
    box <- sync $ drawingAreaNew
    initdata <- sample datafeed
    chartref <- sync $ newIORef (chartmaker initdata)
    let doDraw :: C.Render ()
        doDraw = do
            w <- fromIntegral <$> liftIO (widgetGetAllocatedWidth box)
            h <- fromIntegral <$> liftIO (widgetGetAllocatedHeight box)
            chart <- liftIO $ readIORef chartref
            void $ runBackend (defaultEnv bitmapAlignmentFns) (render (toRenderable chart) (w,h))
    sync $ on box draw doDraw
    callIOStream (\dat -> writeIORef chartref (chartmaker dat) >> widgetQueueDraw box) (toChanges datafeed)
    return box

createLoadableGrammar :: Maybe Window -> Behavior (S.Set T.Text) -> EvStream PhonoGrammar -> Now (VBox, Behavior (Maybe PhonoGrammar))
createLoadableGrammar transwin validsegs extreplace = do

    (grammarLoaded, loadReplace) <- callbackStream
    let grammarChanged = merge grammarLoaded (fmap Just extreplace)
    currentGrammar <- sample $ fromChanges Nothing grammarChanged

    let lenData = fmap (fmap lengthDist) currentGrammar
        lenChart mdat = layout where
            layout = def & layout_x_axis . laxis_title .~ "Word Length (Segments)"
                         & layout_y_axis . laxis_title .~ "Frequency"
                         & layout_x_axis . laxis_override .~ axisGridHide
                         & layout_left_axis_visibility .~ AxisVisibility True False False
                         & layout_y_axis . laxis_override .~ (\ad -> ad{_axis_grid = [], _axis_ticks = [], _axis_labels = []})
                         & layout_legend .~ Nothing
                         & layout_plots .~ fmap plotBars mbars
            mbars = case mdat of
                Nothing -> []
                Just arr -> let
                    total :: Double = fromIntegral (sum arr)
                    bardata = fmap (\(x,y) -> (x, [fromIntegral y / total])) (assocs arr)
                    barplot = def & plot_bars_titles .~ [""]
                                  & plot_bars_values .~ bardata
                                  & plot_bars_spacing .~ BarsFixGap 0 5
                    in [barplot]

    let rulelist (PhonoGrammar _ cs ws) = reverse (zip cs (coords ws))
        rulefeed = fmap (maybe [] rulelist) currentGrammar
        gvalid = do
            segs <- validsegs
            let allValid (ClassGlob _ _ gp) = all (\(_,NClass _ fs) -> all (\(_,f) -> S.member f segs) fs) gp
            cg <- currentGrammar
            return $ fmap (\(PhonoGrammar _ g _) -> all allValid g) cg
        validdesc = ffor gvalid $ \case
            Just True -> ""
            Just False -> "Invalid Features Detected, will ignore"
            Nothing -> "No Grammar Loaded"

    lenchartdisplay <- createChartWidget lenData lenChart
    validlabel <- createLabelDisplay validdesc
    (ruledisplay,_) <- createListDisplay rulefeed $ do
        tvColumn "Weight" $ tvTextDisplay (\(_,w) -> T.pack (showFFloat (Just 2) w []))
        tvColumn "Constraint" $ tvShowDisplay fst

    (loadButton, loadPressed) <- createButton (Just "document-open") (Just "Load Grammar")
    (saveButton, savePressed) <- createButton (Just "document-save") (Just "Save Grammar")
    setAttr widgetSensitive saveButton (fmap isJust currentGrammar)

    vb <- createVBox 2 $ do
        bpack =<< createFrame ShadowIn =<< set' [widgetHeightRequest := 125] lenchartdisplay
        bstretch  =<< createFrame ShadowIn =<< createScrolledWindow ruledisplay
        bpack <=< createHBox 2 $ do
            bstretch =<< set' [widgetWidthRequest := 125, widgetMarginRight := 10] validlabel
            bpack loadButton
            bpack saveButton

    {-txtfilter <- sync fileFilterNew
    allfilter <- sync fileFilterNew

    sync $ do
        fileFilterAddMimeType txtfilter "text/*"
        fileFilterSetName txtfilter "Text Files"
        fileFilterAddPattern allfilter "*"
        fileFilterSetName allfilter "All Files"
        -}

    loadDialog <- sync $ fileChooserDialogNew (Just "Load Grammar") transwin FileChooserActionOpen
        [("gtk-cancel", ResponseCancel), ("gtk-open", ResponseAccept)]
    --sync $ fileChooserAddFilter loadDialog txtfilter
    --sync $ fileChooserAddFilter loadDialog allfilter
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
    --sync $ fileChooserAddFilter saveDialog txtfilter
    --sync $ fileChooserAddFilter saveDialog allfilter
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
