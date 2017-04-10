{-# LANGUAGE LambdaCase, OverloadedStrings, ExtendedDefaultRules, TupleSections #-}
module LearnerControls where

import Graphics.UI.Gtk
import Control.FRPNow hiding (swap, when)
import Control.FRPNow.GTK
import Control.Monad
import Control.Monad.Trans
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Foldable
import Data.Monoid
import Data.Tuple
import Data.Tuple.Select
import Data.Maybe
import Text.PhonotacticLearner.PhonotacticConstraints
import Text.PhonotacticLearner.PhonotacticConstraints.Generators
import Text.PhonotacticLearner.PhonotacticConstraints.FileFormats
import Text.PhonotacticLearner.MaxentGrammar
import Text.PhonotacticLearner
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.ByteString as B
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Data.Array.IArray
import Control.DeepSeq
import Control.Arrow
import Text.Read (readMaybe)
import GtkUtils

default (T.Text)


mapBAsync :: (Eq a) => Event () -> b -> (a -> b) -> Behavior a -> Now (Behavior b, Behavior Bool)
mapBAsync stop yinit f xb = do
    xinit <- sample xb
    let xchanged = toChanges xb `beforeEs` stop
    (pendingSet, setPending) <- callbackStream
    (yset, sety) <- callbackStream
    pending <- sample $ fromChanges True (merge (False <$ yset) pendingSet)
    q <- sync $ newMVar xinit
    flip callStream xchanged $ \xs -> let
        x = last xs
        in sync $ tryTakeMVar q >> putMVar q x
    sync . forkIO . forever $ do
        x <- takeMVar q
        setPending True
        y <- evaluate (f x)
        sety y
        setPending False
    yb <- sample $ fromChanges yinit yset
    return (yb, pending)



createPhonotacticLearnerWidget :: Behavior (FeatureTable String)
                               -> Behavior [LexRow]
                               -> Behavior (Maybe PhonoGrammar)
                               -> Now (Frame, EvStream [LexRow], EvStream PhonoGrammar)
createPhonotacticLearnerWidget dynft dynlex dyngrammar = do
    let dynfeats = fmap (elems . featNames) dynft
        coreClass :: T.Text -> T.Text
        coreClass f = "[±" <> f <> "]"
        dynCoreItems = fmap (fmap (id &&& coreClass)) dynfeats
    fr <- sync frameNew
    done <- getUnrealize fr

    (threshEntry,threshText) <- createEntry "[0.01,0.1,0.2,0.3]"
    (cbEdges,useEdges) <- createCheckButton "Allow word-boundary constraints" False
    (cbTrigrams,useTrigrams) <- createCheckButton "Allow trigram constraints using these core classes:" False
    (listTrigrams,trigramsCore) <- createDynamicChecklist dynCoreItems
    (cbBroken,useBroken ) <- createCheckButton "Allow trigram constraints using these separator classes:" False
    (listBroken,brokenCore) <- createDynamicChecklist dynCoreItems

    setAttr widgetSensitive listTrigrams useTrigrams
    setAttr widgetSensitive listBroken useBroken

    let candsettings = do
            ft <- dynft
            ue <- useEdges
            utri <- useTrigrams
            mtri <- if utri then fmap Just trigramsCore else pure Nothing
            ubrk <- useBroken
            mbrk <- if ubrk then fmap Just brokenCore else pure Nothing
            return (ft, CandidateSettings ue mtri mbrk)
    (candidateData,candidatePending) <- mapBAsync done (0,0,[]) (force . uncurry candidateGrammar) candsettings

    ugSpinner <- sync $ spinnerNew
    setAttr spinnerActive ugSpinner candidatePending
    ugHeader <- createLabel $ do
        (nclasses,ncand,_) <- candidateData
        return $ "UG currently contains " <> T.pack (show nclasses) <> " classes and " <> T.pack (show ncand) <> " candidates."

    sync $ do
        set ugHeader [miscXalign := 0]
        set listTrigrams [widgetMarginLeft := 20]
        set listBroken [widgetMarginLeft := 20]
        scr <- scrolledWindowNew Nothing Nothing
        box <- createVBox $ do
            bpack <=<  createHBox $ do
                bstretch ugHeader
                bpack ugSpinner
            bpack cbEdges
            bpack cbTrigrams
            bpack listTrigrams
            bpack cbBroken
            bpack listBroken

        containerAdd fr scr
        scrolledWindowAddWithViewport scr box
    return (fr, emptyEs, emptyEs)
