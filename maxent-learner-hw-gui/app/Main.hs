{-# LANGUAGE TypeOperators, RecursiveDo, ScopedTypeVariables, TemplateHaskell, QuasiQuotes, OverloadedStrings, ExtendedDefaultRules #-}

module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.CssProvider
import Graphics.UI.Gtk.General.StyleContext
import Graphics.UI.Gtk.Abstract.Widget
import Control.FRPNow
import Control.FRPNow.GTK
import Control.FRPNow.GTK.MissingFFI
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Monad.Trans
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Text.PhonotacticLearner.PhonotacticConstraints
import Data.Array.IArray
import Data.FileEmbed
import Text.RawString.QQ
import Data.Maybe
import FeatureTableEditor
import LexiconEditor
import GrammarEditor
import LearnerControls
import System.IO

default (T.Text)

ipaft :: FeatureTable String
ipaft = fromJust . csvToFeatureTable id . T.unpack . T.decodeUtf8 $ $(embedFile "app/ft-ipa.csv")

css :: T.Text
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
main = runNowGTK $ do
    sync $ hSetEncoding stdout utf8
    sync $ hSetBuffering stdout LineBuffering
    -- example gtk app
    -- initialization code
    window <- sync $ windowNew
    sync $ set window [windowTitle := "Hayes/Wilson Phonotactic Learner."]

    rec (fteditor, dynft) <- createEditableFT (Just window) ipaft
        (lexeditor, dynlex) <- createEditableLexicon (Just window) (fmap segsFromFt dynft) lexout
        (grammareditor, dyngrammar) <- createLoadableGrammar (Just window) (fmap (M.keysSet . featLookup) dynft) grammarout
        (controls,pbar,lexout,grammarout) <- createPhonotacticLearnerWidget dynft dynlex dyngrammar

    sync $ do
        sp <- cssProviderNew
        cssProviderLoadFromString sp css
        thescreen <- widgetGetScreen window
        styleContextAddProviderForScreen thescreen sp 600

        centerpanes <- set' [panedWideHandle := True] =<< createVPaned controls fteditor
        rpanes <- set' [panedWideHandle := True] =<< createHPaned centerpanes grammareditor
        lpanes <- set' [panedWideHandle := True] =<< createHPaned lexeditor rpanes
        box <- createVBox 0 $ do
            bstretch lpanes
            bpack =<< liftIO (vSeparatorNew)
            bpack pbar

        containerAdd window box

    sync $ window `on` deleteEvent $ liftIO mainQuit >> return False

    sync $ widgetShowAll window
