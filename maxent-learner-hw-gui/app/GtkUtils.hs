{-# LANGUAGE LambdaCase, OverloadedStrings, ExtendedDefaultRules #-}

module GtkUtils where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.StyleContext
import Control.FRPNow hiding (swap)
import Control.FRPNow.GTK
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Exception
import Data.Tuple
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Array.IArray
import qualified Data.Map.Lazy as M
import qualified Data.ByteString as B
import System.IO
import Control.DeepSeq
import System.Glib.GObject
import Foreign
import Foreign.C

default (T.Text)

foreign import ccall safe "gtk_scrolled_window_set_overlay_scrolling"
  gtk_scrolled_window_set_overlay_scrolling :: Ptr GObject -> CInt -> (IO ())

scrolledWindowDisableOverlay :: ScrolledWindow -> IO ()
scrolledWindowDisableOverlay sw = let fp = unGObject (toGObject sw)
    in withForeignPtr fp $ \p -> gtk_scrolled_window_set_overlay_scrolling p 0

nothingOnIOError :: IOError -> IO (Maybe a)
nothingOnIOError _ = return Nothing

widgetAddClasses :: WidgetClass widget => widget -> [T.Text] -> IO ()
widgetAddClasses w cs = do
    sc <- widgetGetStyleContext w
    forM_ cs $ \c -> styleContextAddClass sc c
