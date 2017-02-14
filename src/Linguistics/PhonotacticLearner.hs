{-# LANGUAGE ScopedTypeVariables, ExplicitForAll #-}

{-|
Module: Linguistics.PhonotacticLearner
Description: Utility for automatically inferring a maxent grammar form a candidate constraint set and lexicon.
Copyright: © 2016-2017 George Steel and Peter Jurgec
License: GPL-2+
Maintainer: george.steel@gmail.com

Main learning algorithm for inferring granmmar from constraint set and lexicon. To set up the parameters, you will need to use 'sortLexicon' to prepare the text input some way of generating a set of constraint candidates (reperssented as 'DFST's). The PhonotacticConstraints module and its Generators submodule will be useful here.
-}

module Linguistics.PhonotacticLearner(
    generateGrammarIO,

    segmentFiero, joinFiero,
) where

import Linguistics.PhonotacticLearner.Util.Ring
import Linguistics.PhonotacticLearner.Util.Probability
import Linguistics.PhonotacticLearner.DFST
import Linguistics.PhonotacticLearner.MaxentGrammar

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

-- | Given a set of possible segments and a string, break a string into segments.
-- Uses the rules in Fiero orthography (a phonetic writing system using ASCII characters) where the longest possible match is always taken and apostrophes are used as a digraph break.
segmentFiero :: [String] -- All possible segments
             -> String -- Raw text
             -> [String] -- Segmented text
segmentFiero [] = error "Empty segment list."
segmentFiero allsegs = go msl where
    msl = maximum . fmap length $ allsegs
    go _ [] = []
    go _ ('\'':xs) = go msl xs
    go 0 (x:xs) = go msl xs
    go len xs | seg `elem` allsegs = seg : go msl rest
              | otherwise = go (len-1) xs
        where (seg,rest) = splitAt len xs

-- | Joins segments together using Fiero rules. Inserts apostrophes where necerssary.
joinFiero :: [String] -- All possible segments
          -> [String] -- Segmented text
          -> String -- Raw text
joinFiero allsegs = go where
    msl = maximum . fmap length $ allsegs
    go [] = []
    go [x] = x
    go (x:xs@(y:_)) = let z = x++y
                      in  if any (\s -> isPrefixOf s z && not (isPrefixOf s x)) allsegs
                          then x ++ ('\'' : go xs)
                          else x ++ go xs
