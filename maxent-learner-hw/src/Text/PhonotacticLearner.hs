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
    generateGrammarIO, generateGrammarCB
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
import Data.Foldable
import Data.Array.IArray
import System.IO
import Control.Exception
import Control.Parallel


stopsigint :: AsyncException -> IO ()
stopsigint e = case e of
    UserInterrupt -> do
        putStrLn "\n\nInterrupted!"
        return ()
    _ -> throw e

parzip :: [b] -> [a] -> [a]
parzip _ [] = []
parzip [] xs = xs
parzip (y:ys) (x:xs) = (y `par` x):(parzip ys xs)

parAhead :: Int -> [a] -> [a]
parAhead n xs = parzip (drop n xs) xs
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
    -> IO (Array Length Int, [clabel], MulticountDFST sigma, Vec) -- ^ Computed grammar

generateGrammarIO samplesize thresholds candidates wfs = do
    let cbound = psegBounds . snd . head $ candidates
        blankdfa = pruneAndPack (nildfa cbound)
    grammarref <- newIORef (accumArray (+) 0 (0,0) [], [], blankdfa, zero)
    let progresscb _ _ = return ()
        grammarcb lenarr rules dfa ws = mask_ $ atomicWriteIORef grammarref (lenarr,rules,dfa,ws)
    void (generateGrammarCB progresscb grammarcb samplesize thresholds candidates wfs) `catch` stopsigint
    readIORef grammarref

{-|
Like 'generateGrammarIO' but calls callbacks to report progress and non-final grammars. Useful to run in the background and display intermediate results
Does not catch SIGINT.
-}
generateGrammarCB :: forall clabel sigma . (Show clabel, Ix sigma, NFData sigma, NFData clabel, Eq clabel)
    => (Int -> Int -> IO ()) -- ^ callback for reporting progress
    -> (Array Length Int -> [clabel] -> MulticountDFST sigma -> Vec -> IO ()) -- ^ callback for reporting grammar progress
    -> Int -- ^ Monte Carlo sample size
    -> [Double] -- ^ List of accuracy thresholds
    -> [(clabel, ShortDFST sigma)] -- ^ List of candidate constraints. Labels must be unique. All DFSTs must share the same input bounds.
    -> [([sigma],Int)] -- ^ Corpus of sample words and their relative frequencies
    -> IO ([clabel], MulticountDFST sigma, Vec) -- ^ Computed grammar
generateGrammarCB progresscb grammarcb samplesize thresholds candidates wfs = do
    let lwfs = sortLexicon wfs
        cbound = psegBounds . snd . head $ candidates
        blankdfa = pruneAndPack (nildfa cbound)
        lendist = lengthCdf lwfs
        lenarr = lengthFreqs lwfs
        pwfs = packMultiText cbound (wordFreqs lwfs)
        violcand (cl,cdfa) = let o = fromIntegral $ transducePackedShort cdfa pwfs
                             in o `seq` (cl,cdfa,o)
        vcands = parAhead 16 (fmap violcand candidates)

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
        markprg = do
            p <- readIORef passctr
            c <- readIORef candctr
            progresscb p c

    let genSalad :: MulticountDFST sigma -> Vec -> IO (PackedText sigma)
        genSalad dfa weights = do
            salad <- getStdRandom . runState $ sampleWordSalad (fmap (maxentProb weights) (unpackDFA dfa)) lendist samplesize
            evaluate . packMultiText cbound . wordFreqs . sortLexicon . fmap (\x -> (x,1)) $ salad

        processcand :: Double -> (PackedText sigma, [clabel], MulticountDFST sigma, Vec) -> (clabel, ShortDFST sigma, Int) -> IO (PackedText sigma, [clabel], MulticountDFST sigma, Vec)
        processcand thresh grammar@(salad,rules,dfa,ws) (cl,cdfa,o) = do
            markcand
            let o' = fromIntegral $ transducePackedShort cdfa salad
                e = o' * fromIntegral (totalWords lwfs) / fromIntegral samplesize
            score <- evaluate $ upperConfidenceOE (fromIntegral o) e
            if score < thresh && cl `notElem` rules then do
                markprg
                hPutStrLn stderr ""
                putStrLn $ "\nSelected Constraint " ++ show cl ++  " (score=" ++ showFFloat (Just 4) score [] ++ ", o=" ++ show o ++ ", e=" ++ showFFloat (Just 1) e [] ++ ")."

                let rules' = cl:rules
                dfa' <- evaluate . pruneAndPack $ rawIntersection consMC (unpackDFA cdfa) (unpackDFA dfa)
                putStrLn $ "New grammar has " ++ show (length rules') ++ " constraints and " ++ show (numStates dfa') ++ " states."
                ws' <- evaluate . force $ llpOptimizeWeights (lengthFreqs lwfs) pwfs dfa' (consVec 0 ws)
                hPutStrLn stderr ""
                putStrLn $ "Recalculated weights: " ++ showFVec (Just 2) ws'
                grammarcb lenarr rules' dfa' ws'
                salad' <- genSalad dfa' ws'
                return (salad',rules',dfa',ws')
            else return grammar

        processpass :: (PackedText sigma, [clabel], MulticountDFST sigma, Vec) -> Double -> IO (PackedText sigma, [clabel], MulticountDFST sigma, Vec)
        processpass grammar thresh = do
            markpass
            putStrLn $ "\n\n\nStarting pass with threshold " ++ showFFloat (Just 3) thresh ""
            foldlM (processcand thresh) grammar vcands

    initsalad <- genSalad blankdfa zero
    let initgrammar = (initsalad,[],blankdfa,zero)
    (_,finalrules,finaldfa,finalweights) <- foldlM processpass initgrammar thresholds
    putStrLn "\n\n\nAll Pases Complete."
    return (finalrules,finaldfa,finalweights)
