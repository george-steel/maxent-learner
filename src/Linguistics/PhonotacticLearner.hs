{-# LANGUAGE ScopedTypeVariables, ExplicitForAll #-}

{-|
Module: Linguistics.PhonotacticLearner
Description: Utility for automatically inferring a maxent grammar form a candidate constraint set and lexicon.
License: GPL-2+
Copyright: © 2016-2017 George Steel and Peter Jurgec
Maintainer: george.steel@gmail.com

Main entry point of
-}

module Linguistics.PhonotacticLearner(
    generateGrammarIO,
) where

import Linguistics.PhonotacticLearner.Util.Ring
import Linguistics.PhonotacticLearner.Util.Probability
import Linguistics.PhonotacticLearner.WeightedDFA
import Linguistics.PhonotacticLearner.MaxentGrammar
import Linguistics.PhonotacticLearner.WeightOptimizer

import System.Random
import Control.Monad.State
import Control.DeepSeq
import Data.Ix
import Numeric
import Data.IORef
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

This algorithm by repeatedly taking the next constraint on the list of candidates which has an observed/expected number of violations under a threshold value, adding it to the current constraint set, and then reweigthing the constraint to refine the grammar. Multiple passes are made with an increasing sequence of threshold values.

Each constraint in the candidate is reperesemted as a pair consisting of a unique label and a DFST which counts the violations of that constraint. The corpus of words is reperesented as a 'Lexicon' which can be generated from a list of words by 'sortLexicon'. The grammar is output as a list of constraint labels, a merged DFST to count violations, and a 'Vec' of weights.

To generate a constraint candidate list, several UG functions are contained in the 'UniversalGrammar' module
-}
generateGrammarIO :: forall g clabel sigma . (Show clabel, Ix sigma, NFData sigma, NFData clabel, Eq clabel)
    => Int -- ^ Monte Carlo sample size
    -> [Double] -- ^ list of accuracy thresholds
    -> [(clabel, ShortDFST sigma)] -- ^ List of candidate constraints. Labels must be unique. All DFSTs must share the same input bounds.
    -> Lexicon sigma -- ^ corpus of sample words and their relative frequencies
    -> IO ([clabel], MulticountDFST sigma, Vec) -- ^ computed grammar
generateGrammarIO samplesize thresholds candidates wfs = do
    let cbound = psegBounds . snd . head $ candidates
        blankdfa = nildfa cbound
        lendist = lengthCdf wfs
        pwfs = packMultiText cbound (wordFreqs wfs)

    hashctr :: IORef Int <- newIORef 0
    let mark500 = do
            c <- readIORef hashctr
            when (c `mod` 500 == 0) $ do
                hPutStr stderr "#"
                hFlush stderr
            modifyIORef' hashctr (+1)

    currentGrammar :: IORef ([clabel], MulticountDFST sigma, Vec) <- newIORef ([],pruneAndPack blankdfa ,zero)

    let genSalad :: IO (PackedText sigma)
        genSalad = do
            (_,dfa,weights) <- readIORef currentGrammar
            salad' <- getStdRandom . runState $ sampleWordSalad (fmap (maxentProb weights) (unpackDFA dfa)) lendist samplesize
            return . packMultiText cbound . wordFreqs . sortLexicon . fmap (\x -> (x,1)) $ salad'

    currentSalad <- newIORef undefined

    handle stopsigint $ do
        forM_ thresholds $ \accuracy -> do
            putStrLn $ "\n\n\nStarting pass with threshold " ++ showFFloat (Just 3) accuracy ""
            writeIORef currentSalad =<< genSalad
            forM_ candidates $ \(cl,cdfa) -> do
                mark500
                (grammar, dfa, weights) <- readIORef currentGrammar
                salad <- readIORef currentSalad
                let o = fromIntegral $ transducePackedShort cdfa pwfs
                    o' = fromIntegral $ transducePackedShort cdfa salad
                    e = o' * fromIntegral (totalWords wfs) / fromIntegral samplesize
                    score = upperConfidenceOE o e

                when (score < accuracy && cl `notElem` grammar) $ do
                    hPutStrLn stderr ""
                    putStrLn $ "\nSelected Constraint " ++ show cl ++  " (score=" ++ showFFloat (Just 4) score [] ++ ", o=" ++ showFFloat (Just 1) o [] ++ ", e=" ++ showFFloat (Just 1) e [] ++ ")."
                    let newgrammar = cl:grammar
                        newdfa :: MulticountDFST sigma = pruneAndPack (rawIntersection consMC (unpackDFA cdfa) (unpackDFA dfa))
                    putStrLn $ "New grammar has " ++ show (length newgrammar) ++ " constraints and " ++ show (numStates newdfa) ++ " states."
                    let oldweights = consVec 0 weights
                    newweights <- evaluate . force $ llpOptimizeWeights (lengthFreqs wfs) pwfs newdfa oldweights
                    hPutStrLn stderr ""
                    putStrLn $ "Recalculated weights: " ++ showFVec (Just 2) newweights
                    atomicWriteIORef currentGrammar . force $ (newgrammar, newdfa, newweights)
                    writeIORef currentSalad =<< genSalad
        putStrLn "\n\n\nAll Pases Complete."

    readIORef currentGrammar
