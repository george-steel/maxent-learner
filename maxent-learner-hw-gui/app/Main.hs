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
import FeatureTableEditor
import LexiconEditor

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

main :: IO ()
main = runNowGTK $ mdo
    -- example gtk app
    -- initialization code
    window <- sync $ windowNew
    (fteditor,dynft) <- createEditableFT (Just window) ipaft
    (lexeditor,dynlex) <- createEditableLexicon (Just window) (fmap segsFromFt dynft) emptyEs
    fmat <- displayDynFeatureTable dynft
    sync $ do
        panes <- hPanedNew
        sp <- cssProviderNew
        cssProviderLoadFromString sp css
        thescreen <- widgetGetScreen window
        styleContextAddProviderForScreen thescreen sp 600
        fr <- frameNew
        set fr [frameShadowType := ShadowIn ]
        containerAdd fr fmat
        vb <- vBoxNew False 0
        boxPackStart vb fteditor PackGrow 0
        boxPackStart vb fr PackGrow 0
        panedAdd1 panes vb
        panedAdd2 panes lexeditor
        containerAdd window panes

    sync $ window `on` deleteEvent $ liftIO mainQuit >> return False

    sync $ widgetShowAll window



createFileChooserButton :: String -> Now (FileChooserButton, Behavior (Maybe FilePath))
createFileChooserButton title = do
    fsb <- sync $ fileChooserButtonNew title FileChooserActionOpen
    fsev <- getSignal fileChooserButtonFileSet fsb (fileChooserGetFilename fsb >>=)
    selfile <- sample $ fromChanges Nothing fsev
    return (fsb,selfile)
