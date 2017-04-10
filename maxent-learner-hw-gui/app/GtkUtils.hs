{-# LANGUAGE LambdaCase, OverloadedStrings, ExtendedDefaultRules #-}

module GtkUtils where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.StyleContext
import Control.FRPNow hiding (swap)
import Control.FRPNow.GTK
import Control.Monad.Reader
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import Control.DeepSeq
import System.Glib.GObject
import Foreign
import Foreign.C

default (T.Text)

foreign import ccall safe "gtk_scrolled_window_set_overlay_scrolling"
  gtk_scrolled_window_set_overlay_scrolling :: Ptr GObject -> CInt -> IO ()

scrolledWindowDisableOverlay :: ScrolledWindow -> IO ()
scrolledWindowDisableOverlay sw = let fp = unGObject (toGObject sw)
    in withForeignPtr fp $ \p -> gtk_scrolled_window_set_overlay_scrolling p 0

nothingOnIOError :: IOError -> IO (Maybe a)
nothingOnIOError _ = return Nothing

widgetAddClasses :: WidgetClass widget => widget -> [T.Text] -> IO ()
widgetAddClasses w cs = do
    sc <- widgetGetStyleContext w
    forM_ cs $ \c -> styleContextAddClass sc c

createHPaned :: (WidgetClass w1, WidgetClass w2) => w1 -> w2 -> IO (HPaned)
createHPaned l r = do
    p <- hPanedNew
    panedPack1 p l True False
    panedPack2 p r True False
    return p

createVPaned :: (WidgetClass w1, WidgetClass w2) => w1 -> w2 -> IO (VPaned)
createVPaned l r = do
    p <- vPanedNew
    panedPack1 p l True False
    panedPack2 p r True False
    return p

bpack :: (WidgetClass w, BoxClass b) => w -> ReaderT b IO ()
bpack w = ReaderT $ \b -> boxPackStart b w PackNatural 0

bstretch :: (WidgetClass w, BoxClass b) => w -> ReaderT b IO ()
bstretch w = ReaderT $ \b -> boxPackStart b w PackGrow 0

bspacer :: (BoxClass b) => ReaderT b IO ()
bspacer = ReaderT $ \b -> do
    s <- hBoxNew False 0
    boxPackStart b s PackGrow 10

createHBox :: (MonadIO m) => ReaderT HBox IO () -> m HBox
createHBox filler = liftIO $ do
    b <- hBoxNew False 0
    runReaderT filler b
    return b

createVBox :: (MonadIO m) => ReaderT VBox IO () -> m VBox
createVBox filler = liftIO $ do
    b <- vBoxNew False 0
    runReaderT filler b
    return b
