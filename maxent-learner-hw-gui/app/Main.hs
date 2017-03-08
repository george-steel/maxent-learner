{-# LANGUAGE TypeOperators, RecursiveDo, ScopedTypeVariables, TemplateHaskell, QuasiQuotes, OverloadedStrings #-}

module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.CssProvider
import Graphics.UI.Gtk.General.StyleContext
import Graphics.UI.Gtk.Abstract.Widget
import Control.FRPNow
import Control.FRPNow.GTK
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Monad.Trans
import qualified Data.Text as T
import Text.PhonotacticLearner.PhonotacticConstraints
import Data.Array.IArray
import Data.FileEmbed
import Text.RawString.QQ
import Data.Maybe

ipaft :: FeatureTable String
ipaft = fromJust (csvToFeatureTable id $(embedStringFile "./app/ft-ipa.csv"))

css :: String
css = [r|
#featuretable{
    background-color: @theme_base_color;
    padding: 5px;
    font-size: 80%;
}
#featuretable label {
    padding: 0 2px;
}
#featuretable .oddcol {
    background-color: mix(@theme_base_color,@theme_bg_color,0.5);
}
#featuretable .segheader {font-weight: bold;}
#featuretable .featheader {font-weight: bold;}
#featuretable .featzero {color: mix(@theme_fg_color, transparent, 0.35);}
|]

jt :: T.Text -> Maybe T.Text
jt = Just

widgetAddClasses :: WidgetClass widget => widget -> [T.Text] -> IO ()
widgetAddClasses w cs = do
    sc <- widgetGetStyleContext w
    forM_ cs $ \c -> styleContextAddClass sc c

main :: IO ()
main = runNowGTK $ mdo
    -- example gtk app
    -- initialization code
    window <- sync $ windowNew

    fmat <- sync $ displayFeatureMatrix ipaft
    sync $ do
        sp <- cssProviderNew
        cssProviderLoadFromString sp css
        thescreen <- widgetGetScreen window
        styleContextAddProviderForScreen thescreen sp 600
        scr <- scrolledWindowNew Nothing Nothing
        fr <- frameNew
        set fr [frameShadowType := ShadowIn ]
        --set scr [widgetName := Just "ftcontainer"]
        containerAdd window fr
        containerAdd fr scr
        --set scr [ containerBorderWidth := 10 ]
        containerAdd scr fmat

    sync $ window `on` deleteEvent $ liftIO mainQuit >> return False

    sync $ widgetShowAll window

displayFeatureMatrix :: FeatureTable String -> IO Grid
displayFeatureMatrix ft = do
    g <- gridNew
    set g [widgetName := jt "featuretable"]
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
            FPlus -> labelNew (jt "+")
            FMinus -> labelNew (jt "−")
            FOff -> do
                l <- labelNew (jt "0")
                widgetAddClasses l ["featzero"]
                return l
        let oddclass = if odd s then ["oddcol"] else []
        widgetAddClasses l oddclass
        gridAttach g l s f 1 1
    return g
