{-# LANGUAGE ScopedTypeVariables, ExplicitForAll #-}

{-|
Module: Text.PhonotacticLearner
Description: Utility for automatically inferring a maxent grammar form a candidate constraint set and lexicon.
Copyright: © 2016-2017 George Steel and Peter Jurgec
License: GPL-2+
Maintainer: george.steel@gmail.com

Main learning algorithm for inferring granmmar from constraint set and lexicon. To set up the parameters, you will need to use 'sortLexicon' to prepare the text input some way of generating a set of constraint candidates (reperssented as 'DFST's). The PhonotacticConstraints module and its Generators submodule will be useful here.
-}

module Text.PhonotacticLearner(
    generateGrammarIO, generateGrammarIOCB
) where

import Text.PhonotacticLearner.Util.Ring
import Text.PhonotacticLearner.Util.Probability
import Text.PhonotacticLearner.DFST
import Text.PhonotacticLearner.MaxentGrammar

import System.Random
import Control.Monad.State
import Control.DeepSeq
import Data.Ix
import Numeric
import Data.IORef
import Data.List
import System.IO
import Control.Exception


stopsigint :: AsyncException -> IO ()
stopsigint e = case e of
    UserInterrupt -> do
        putStrLn "\n\nInterrupted!"
        return ()
    _ -> throw e

{-|
Infer a phonotactic grammar from a list of candidate constraints and a corpus of texts.

This algorithm works by keeping a running grammar (starting with an empty one) and repeatedly taking the next constraint on the list of candidates which improves the grammar by a large enough margin. Constraints are selected for inclusion when they have an observed number of violations less then a threshold factor times the expected number of violations in the running grammar. After each constraint is added, the weights of the running grammar are optimized.  Multiple passes are made with an increasing sequence of thresholds.

For maximum flexibility, constraints are reperesented as DFSTs which count the number of violations of the constraint in an input string. In order to have understandable output, each candadite DFST is paired with a unique label, with the final grammar output as a merged DFST, a list of selected labels, and a weight vector. If the constraints are generated from a list of globs, aplying @(id &&& 'matchCounter')@ to the list will produce suitable candidates.

Since we need to process these words using DFSTs, the set of valid segments must form a continous 'Ix' range, such as @[\'a\'..\'z\']@. If the segments do not, please replace them with indices into a lookup table.

Since the algorithm works by continuous refinement, this action will catch SIGINT and terminate early if the signal is received.
-}
generateGrammarIO :: forall clabel sigma . (Show clabel, Ix sigma, NFData sigma, NFData clabel, Eq clabel)
    => Int -- ^ Monte Carlo sample size
    -> [Double] -- ^ List of accuracy thresholds
    -> [(clabel, ShortDFST sigma)] -- ^ List of candidate constraints. Labels must be unique. All DFSTs must share the same input bounds.
    -> [([sigma],Int)] -- ^ Corpus of sample words and their relative frequencies
    -> IO ([clabel], MulticountDFST sigma, Vec) -- ^ Computed grammar

generateGrammarIO = generateGrammarIOCB (\_ _ -> return ()) (\_ _ -> return ())

generateGrammarIOCB :: forall clabel sigma . (Show clabel, Ix sigma, NFData sigma, NFData clabel, Eq clabel)
    => (Int -> Int -> IO ()) -- ^ callback for reporting progress
    -> ([clabel] -> Vec -> IO ()) -- ^ callback for reporting grammar progress
    -> Int -- ^ Monte Carlo sample size
    -> [Double] -- ^ List of accuracy thresholds
    -> [(clabel, ShortDFST sigma)] -- ^ List of candidate constraints. Labels must be unique. All DFSTs must share the same input bounds.
    -> [([sigma],Int)] -- ^ Corpus of sample words and their relative frequencies
    -> IO ([clabel], MulticountDFST sigma, Vec) -- ^ Computed grammar
generateGrammarIOCB progresscb grammarcb samplesize thresholds candidates wfs = do
    let lwfs = sortLexicon wfs
        cbound = psegBounds . snd . head $ candidates
        blankdfa = nildfa cbound
        lendist = lengthCdf lwfs
        pwfs = packMultiText cbound (wordFreqs lwfs)

    passctr :: IORef Int <- newIORef 0
    candctr :: IORef Int <- newIORef 0
    let markpass = do
            modifyIORef' passctr (+1)
            writeIORef candctr 0
            p <- readIORef passctr
            progresscb p 0
        markcand = do
            modifyIORef' candctr (+1)
            c <- readIORef candctr
            when (c `mod` 500 == 0) $ do
                p <- readIORef passctr
                hPutStr stderr "#"
                hFlush stderr
                progresscb p c

    currentGrammar :: IORef ([clabel], MulticountDFST sigma, Vec) <- newIORef ([],pruneAndPack blankdfa ,zero)

    let genSalad :: IO (PackedText sigma)
        genSalad = do
            (_,dfa,weights) <- readIORef currentGrammar
            salad' <- getStdRandom . runState $ sampleWordSalad (fmap (maxentProb weights) (unpackDFA dfa)) lendist samplesize
            return . packMultiText cbound . wordFreqs . sortLexicon . fmap (\x -> (x,1)) $ salad'

    currentSalad <- newIORef undefined

    handle stopsigint $ do
        forM_ thresholds $ \accuracy -> do
            markpass
            putStrLn $ "\n\n\nStarting pass with threshold " ++ showFFloat (Just 3) accuracy ""
            writeIORef currentSalad =<< genSalad
            forM_ candidates $ \(cl,cdfa) -> do
                markcand
                (grammar, dfa, weights) <- readIORef currentGrammar
                salad <- readIORef currentSalad
                let o = fromIntegral $ transducePackedShort cdfa pwfs
                    o' = fromIntegral $ transducePackedShort cdfa salad
                    e = o' * fromIntegral (totalWords lwfs) / fromIntegral samplesize
                    score = upperConfidenceOE o e

                when (score < accuracy && cl `notElem` grammar) $ do
                    hPutStrLn stderr ""
                    putStrLn $ "\nSelected Constraint " ++ show cl ++  " (score=" ++ showFFloat (Just 4) score [] ++ ", o=" ++ showFFloat (Just 1) o [] ++ ", e=" ++ showFFloat (Just 1) e [] ++ ")."
                    let newgrammar = cl:grammar
                        newdfa :: MulticountDFST sigma = pruneAndPack (rawIntersection consMC (unpackDFA cdfa) (unpackDFA dfa))
                    putStrLn $ "New grammar has " ++ show (length newgrammar) ++ " constraints and " ++ show (numStates newdfa) ++ " states."
                    let oldweights = consVec 0 weights
                    newweights <- evaluate . force $ llpOptimizeWeights (lengthFreqs lwfs) pwfs newdfa oldweights
                    hPutStrLn stderr ""
                    putStrLn $ "Recalculated weights: " ++ showFVec (Just 2) newweights
                    atomicWriteIORef currentGrammar . force $ (newgrammar, newdfa, newweights)
                    grammarcb newgrammar newweights
                    writeIORef currentSalad =<< genSalad
        putStrLn "\n\n\nAll Pases Complete."

    readIORef currentGrammar
