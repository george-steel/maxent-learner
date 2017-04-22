{-# LANGUAGE LambdaCase, OverloadedStrings, ExtendedDefaultRules, TupleSections, RecursiveDo #-}
module LearnerControls where

import Graphics.UI.Gtk
import Control.FRPNow as FRP hiding (swap, when)
import Control.FRPNow.GTK
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.State
import Data.Foldable
import Data.Monoid
import Data.Tuple
import Data.Tuple.Select
import Data.Maybe
import Data.List
import Text.PhonotacticLearner.PhonotacticConstraints
import Text.PhonotacticLearner.PhonotacticConstraints.Generators
import Text.PhonotacticLearner.PhonotacticConstraints.FileFormats
import Text.PhonotacticLearner.MaxentGrammar
import Text.PhonotacticLearner.Util.Probability
import Text.PhonotacticLearner.DFST
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
import System.Random (getStdRandom)
import System.IO
import Control.Parallel

default (T.Text)

isSorted :: (Ord a) => [a] -> Bool
isSorted []       = True
isSorted [x]      = True
isSorted (x:xs@(y:_)) = x < y && isSorted xs

parAhead :: [a] -> [a]
parAhead [] = []
parAhead [x] = [x]
parAhead (x:y:xs) = (y `par` x : parAhead (y:xs))

chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = let (y,ys) = splitAt n xs in y : chunkList n ys

parListChunk :: (NFData a) => Int -> [a] -> [a]
parListChunk n = join . parAhead . fmap force . chunkList n

createHBoxHom :: (MonadIO m) => Int -> ReaderT HBox IO () -> m HBox
createHBoxHom spacing filler = liftIO $ do
    b <- hBoxNew True spacing
    runReaderT filler b
    return b

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

beforeE :: Event a -> Event () -> Behavior (Event a)
beforeE ev cutoff = fmap join $ FRP.first (never <$ cutoff) (pure <$> ev)

generateSalad :: (Double -> T.Text -> IO ()) -> FeatureTable String -> PhonoGrammar -> Int -> IO [LexRow]
generateSalad progcb ft (PhonoGrammar lendist rules ws) n = do
    let nrules = length rules
        lencdf = massToCdf (fmap (second fromIntegral) (assocs lendist))
        blankdfa :: MulticountDFST SegRef
        blankdfa = pruneAndPack . nildfa $ srBounds ft
        addRule :: ClassGlob -> (MulticountDFST SegRef, Int) -> IO (MulticountDFST SegRef, Int)
        addRule r (g,n) = do
            g' <- evaluate . pruneAndPack $ rawIntersection consMC (unpackDFA . cgMatchCounter ft $ r) (unpackDFA g)
            progcb (0.95 * (fromIntegral n / fromIntegral nrules)) (T.pack ("Building DFA: " ++ show n ++ "/" ++ show nrules))
            return (g',n+1)

    dfa <- fmap fst $ foldrM addRule (blankdfa,1) rules

    progcb 1 "Running Generator"

    salad <- getStdRandom . runState $ sampleWordSalad (fmap (maxentProb ws) (unpackDFA dfa)) lencdf n

    let saladLex = wordFreqs . sortLexicon . fmap (\x -> (x,1)) $ salad
    return $ fmap (\(w,f) -> LexRow (refsToSegs ft w) f) saladLex



createPhonotacticLearnerWidget :: Behavior (FeatureTable String)
                               -> Behavior [LexRow]
                               -> Behavior (Maybe PhonoGrammar)
                               -> Now (Notebook, HBox, EvStream [LexRow], EvStream PhonoGrammar)
createPhonotacticLearnerWidget dynft dynlex dyngrammar = mdo
    (grammarOut,outputGrammar) <- callbackStream
    (lexOut, outputLex) <- callbackStream

    let dynfeats = fmap (elems . featNames) dynft
        dynCoreItems = fmap (fmap (\f -> (f, "[±" <> f <> "]"))) dynfeats

    (cbEdges,useEdges) <- createCheckButton "Allow word-boundary constraints" False
    (cbTrigrams,useTrigrams) <- createCheckButton "Allow trigram constraints using these core classes:" False
    (listTrigrams,trigramsCore) <- createDynamicChecklist dynCoreItems
    (cbBroken,useBroken) <- createCheckButton "Allow trigram constraints using these separator classes:" False
    (listBroken,brokenCore) <- createDynamicChecklist dynCoreItems

    sync $ set listTrigrams [widgetMarginLeft := 20]
    sync $ set listBroken [widgetMarginLeft := 20]
    setAttr widgetSensitive listTrigrams useTrigrams
    setAttr widgetSensitive listBroken useBroken

    (threshEntry,threshText) <- createFilteredEntry (`elem` ("1234567890. "::String)) "0.01 0.1 0.2 0.3"
    (learnSaladEntry, learnSaladVal) <- createIntSpinEntry (100,10000) 100 3000

    (learnStartButton, learnStartPressed) <- createButton (Just "system-run") (Just "Start")

    (gibberSizeEntry,gibberSizeVal) <- createIntSpinEntry (100,10000) 100 3000
    (gibberStartButton, gibberStartPressed) <- createButton (Just "system-run") (Just "Start")


    let threshVals :: Behavior (Maybe [Double])
        threshVals = ffor threshText $ mfilter (all (<1)) . mfilter isSorted . mapM readMaybe . words
        candsettings = do
            ft <- dynft
            ue <- useEdges
            utri <- useTrigrams
            mtri <- if utri then fmap Just trigramsCore else pure Nothing
            ubrk <- useBroken
            mbrk <- if ubrk then fmap Just brokenCore else pure Nothing
            return (ft, CandidateSettings ue mtri mbrk)
    (candidateData,candidatePending) <- mapBAsync done (0,0,[]) (force . uncurry candidateGrammar) candsettings

    ugSpinner <- createSpinner candidatePending
    ugHeader <- createLabelDisplay . ffor candidateData $ \(nclasses,ncand,_) ->
        "UG currently contains " <> T.pack (show nclasses) <> " classes and " <> T.pack (show ncand) <> " candidates."

    (progressSet, setProgress) <- callbackStream
    progress <- sample $ fromChanges Nothing progressSet
    let isPending = fmap isJust progress

    pbar <- createProgressBar progress
    (stopButton, stopPressed) <- createButton (Just "process-stop") (Just "Stop")
    setAttr widgetSensitive stopButton isPending

    setAttr widgetSensitive gibberStartButton $ do
        mg <- dyngrammar
        p <- isPending
        return $ isJust mg && not p
    setAttr widgetSensitive learnStartButton $ do
        l <- dynlex
        gp <- candidatePending
        p <- isPending
        tv <- threshVals
        return $ not (null l) && not gp && not p && isJust tv

    flip callStream gibberStartPressed $ \_ -> do
        ft <- sample dynft
        mgrammar <- sample dyngrammar
        n <- sample gibberSizeVal
        case mgrammar of
            Just pg -> void $ do
                (tdone, amdone) <- callback
                clickedStop <- sample $ next stopPressed
                sync $ setProgress (Just (0,"Statred Gibbering"))
                thread <- sync . forkIO . flip finally (amdone () >> setProgress Nothing) $ do
                    let progcb p s = mask_ $ setProgress (Just (p,s))
                    salad <- generateSalad progcb ft pg n
                    mask_ $ outputLex salad
                doStop <- sample $ (sync (killThread thread) <$ clickedStop) `beforeE` tdone
                planNow doStop
            Nothing -> return ()

    flip callStream learnStartPressed $ \_ -> do
        ft <- sample dynft
        l <- sample dynlex
        (_,ncands,candglobs) <- sample candidateData
        mthresh <- sample threshVals
        nsalad <- sample learnSaladVal
        when (not (null l) && isJust mthresh) $ do
            let thresh = fromJust mthresh
            (tdone, amdone) <- callback
            clickedStop <- sample $ next stopPressed
            sync $ setProgress (Just (0,"Statred Learning"))
            thread <- sync . forkIO . flip finally (amdone () >> setProgress Nothing) $ let
                nthresh = length thresh
                cands = parListChunk 500 $ fmap (force . (id *** matchCounter)) candglobs
                progresscb pass cand = mask_ $ let
                    prg = (fromIntegral (pass-1) + (fromIntegral cand / fromIntegral ncands)) / fromIntegral nthresh
                    txt = T.pack $ "Pass " ++ show pass ++ "/" ++ show nthresh ++ ": Processing candidate " ++ show cand ++ "/" ++ show ncands ++ "."
                    in setProgress (Just (prg,txt))
                grammarcb lenarr rules _ ws = mask_ $ outputGrammar (PhonoGrammar lenarr rules ws)
                lexlist = ffor l $ \(LexRow w f) -> (segsToRefs ft w, f)
                in void $ generateGrammarCB progresscb grammarcb nsalad thresh cands lexlist
            doStop <- sample $ (sync (killThread thread) <$ clickedStop) `beforeE` tdone
            planNow doStop
            return ()

    let allmargins n = [widgetMarginLeft := n, widgetMarginRight := n, widgetMarginTop := n, widgetMarginBottom := n]
    controls <- createNotebook $ do
        nbpage "Learn" <=< createScrolledViewport <=< set' (allmargins 10) <=< createVBox 5 $ do
            bpack <=< set' [widgetMarginLeft := 5, widgetMarginRight := 5] <=< createHBox 0 $ do
                bstretch ugHeader
                bpack ugSpinner
            bpack cbEdges
            bpack cbTrigrams
            bpack =<< set' [widgetMarginLeft := 20] listTrigrams
            bpack cbBroken
            bpack =<< set' [widgetMarginLeft := 20] listBroken
            bpack <=< createExpander "Advanced Settings" False <=< set' [widgetMarginLeft := 20] <=< createGrid 5 2 $ do
                gcell (1,1) =<< createLabel "Selection thresholds"
                gcell (2,1) =<< set' [widgetHExpand := True] threshEntry
                gcell (1,2) =<< createLabel "Selection salad size"
                gcell (2,2) =<< set' [widgetHExpand := True] learnSaladEntry
            bpack learnStartButton
        nbpage "Gibber" <=< set' (allmargins 10) <=< createVBox 2 $ do
            bpack <=< createHBox 5 $ do
                bpack =<< createLabel "Number of words"
                bstretch gibberSizeEntry
            bspacer
            bpack gibberStartButton

    done <- getUnrealize controls
    progcontrols <- createHBox 2 $ do
        bstretch pbar
        bpack stopButton

    return (controls, progcontrols, lexOut, grammarOut)
