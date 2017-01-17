{-# LANGUAGE ScopedTypeVariables, ExplicitForAll #-}

{-|
Module: Linguistics.PhonotacticLearner
Description: Utility for automatically inferring a maxent grammar form a candidate constraint set and lexicon.
License: GPL-2+
Copyright: © 2016-2017 George Steel and Peter Jurgec
Maintainer: george.steel@gmail.com

-}

module Linguistics.PhonotacticLearner(
    generateGrammarIO
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

{- main entry point for constraint learning.

This function makes no assumptions as to UG and instead, takes in a list of constraint candidates (in order of decreasing generality).
These constraints take the form of a pair consisting of a label (only compared for equality to prevent duplication)
and a DFA which counts the number of times a constraint is violated in a string.

To generate a constraint candidate list, several UG functions are contained in the PhonotacticGrammar module
-}
generateGrammarIO :: forall g clabel sigma . (Show clabel, Ix sigma, NFData sigma, NFData clabel, Eq clabel) -- In the typical case, clabel = ClassGlob, sigma = SegRef
    => Int -- Monte Carlo sample size
    -> [Double] -- list of accuracy thresholds
    -> [(clabel, ShortDFST sigma)] -- list of constraints to try in order. Each constraint has a label and a dfa to compute it
    -> Lexicon sigma -- List of words to try and their frequencies
    -> IO ([clabel], MulticountDFST sigma, Vec) -- computed grammar
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
            salad' <- getStdRandom . runState $ sampleWordSalad (dropCounts . unpackDFA $ weightExpVec dfa weights) lendist samplesize
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
