{-# LANGUAGE ScopedTypeVariables, ExplicitForAll #-}
module ConstraintLearner where

import Ring
import Probability
import WeightedDFA
import MaxentGrammar
import WeightOptimizer
import System.Random
import Control.Monad.State
import Control.DeepSeq
import Data.Ix
import Numeric
import Data.IORef
import System.IO


-- main function to learn a list fo constraints,
generateGrammar :: forall g m clabel sigma . (RandomGen g, MonadState g m, Show clabel, Ix sigma, NFData sigma, NFData sigma, Eq clabel) -- In the typical case, m = StateT g IO, clabel = Glob, sigma = SegRef
    => m () -- action to mark progress
    -> (String -> m ()) -- function to log messages. (liftIO . putStrLn), write, and void are all good canduidates
    -> Int -- Monte Carlo sample size
    -> [Double] -- list of accuracy thresholds
    -> [(clabel, ShortDFST sigma)] -- list of constraints to try in order. Each constraint has a label and a dfa to compute it
    -> Lexicon sigma -- List of words to try and their frequencies
    -> m [(clabel, Double)] -- computed grammar
generateGrammar prog out samplesize thresholds (possibleConstraints) wfs = do
    let cbound = shortSegBounds . snd . head $ possibleConstraints
        blankdfa = nildfa cbound
        lendist = lengthCdf wfs
        pwfs = packMultiText cbound (wordFreqs wfs)

        chooseConstraints :: Double -- accuracy
                          -> [clabel] -- grammar do far
                          -> MaxentViolationCounter sigma -- DFA of grammar so fat
                          -> Vec -- weights
                          -> PackedText sigma -- word salad for evaluating expectations
                          -> [(clabel, ShortDFST sigma)] -- candidates left
                          -> m ([clabel], MaxentViolationCounter sigma, Vec)
        chooseConstraints _ grammar dfa weights _ [] = do
            out "Pass complete."
            return (grammar, dfa, weights)
        chooseConstraints accuracy grammar dfa weights salad ((cl,cdfa):cs)
            | score > accuracy || cl `elem` grammar = do
                --out $ "Rejected " ++ show cl ++ " " ++ showFFloat (Just 4) score [] ++ ": " ++ ([o,o',e] >>= (\f -> showFFloat (Just 0) f " "))
                prog
                chooseConstraints accuracy grammar dfa weights salad cs
            | otherwise = do
                out $ "\nSelected Constraint " ++ show cl ++  " (score " ++ showFFloat (Just 4) score [] ++ ", o=" ++ showFFloat (Just 1) o [] ++ ", e=" ++ showFFloat (Just 1) e [] ++ ")."
                let newgrammar = cl:grammar
                    newdfa = dfaProduct consMC (unpackShortDFST cdfa) dfa
                out $ "New grammar has " ++ (show . rangeSize . stateBounds $ newdfa) ++ " states."
                let oldweights = consVec 0 weights
                    newweights = llpOptimizeWeights wfs newdfa oldweights
                out $ "Recalculated weights: " ++ showFVec (Just 2) newweights
                newsalad' <- fmap force $ sampleWordSalad (dropCounts (weightConstraints newdfa newweights)) lendist samplesize
                let newsalad = packMultiText cbound . wordFreqs . sortLexicon . fmap (\x -> (x,1)) $ newsalad'
                -- out $ "Generated new salad."
                chooseConstraints accuracy newgrammar newdfa newweights newsalad cs
            where
                score = upperConfidenceOE o e
                o = fromIntegral $ transducePacked cdfa pwfs
                o' = fromIntegral $ transducePacked cdfa salad
                e = o' * fromIntegral (totalWords wfs) / fromIntegral samplesize

        accuracyPass :: [clabel] -- grammar do far
                     -> MaxentViolationCounter sigma -- DFA of grammar so fat
                     -> Vec -- weights
                     -> [Double] -- accuracy thresholds
                     -> m ([clabel], MaxentViolationCounter sigma, Vec)
        accuracyPass grammar dfa weights [] = do
            out "All passes complete"
            return (grammar, dfa, weights)
        accuracyPass grammar dfa weights (acc:accs) = do
            out $ "\n\n"
            out $ "Starting pass with threshold " ++ showFFloat (Just 3) acc ""
            salad' <- sampleWordSalad (dropCounts (weightConstraints dfa weights)) lendist samplesize
            let salad = packMultiText cbound . wordFreqs . sortLexicon . fmap (\x -> (x,1)) $ salad'
            (newgrammar, newdfa, newweights) <- chooseConstraints acc grammar dfa weights salad possibleConstraints
            accuracyPass newgrammar newdfa newweights accs

    -- actually call our recursive function
    (grammar, dfa, weights) <- accuracyPass [] blankdfa zero thresholds

    return . reverse $ zip grammar (coords weights)


{- main entry point for constraint learning.

This function makes no assumptions as to UG and instead, takes in a list of constraint candidates (in order of decreasing generality).
These constraints take the form of a pair consisting of a label (only compared for equality to prevent duplication)
and a DFA which counts the number of times a constraint is violated in a string.

To generate a constraint candidate list, several UG functions are contained in the PhonotacticGrammar module
-}
generateGrammarIO :: (Ix sigma, Show clabel, Eq clabel, NFData sigma) => Int -- Monte Carlo sample size
    -> [Double] -- list of accuract thresholds
    -> [(clabel, ShortDFST sigma)] -- list of constraints to try in order. Each constraint has a label and a dfa to compute it
    -> Lexicon sigma -- List of words to try and their frequencies
    -> IO [(clabel, Double)]
generateGrammarIO samplesize thresholds (possibleConstraints) wfs = do
    ctr :: IORef Int <- newIORef 0
    let mark100 = do
        c <- readIORef ctr
        when (c `mod` 100 == 0) $ do
            putStr "#"
            hFlush stdout
        modifyIORef' ctr (+1)
    gen <- newStdGen
    evalStateT (generateGrammar (liftIO mark100) (liftIO . putStrLn) samplesize thresholds (possibleConstraints) wfs) gen
