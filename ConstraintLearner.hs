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

evaluateConstraint :: (Ix sigma) => Lexicon sigma -> Lexicon sigma -> SingleViolationCounter sigma -> Double
evaluateConstraint wfs salad dfa = upperConfidenceOE o e
    where
        o = observedViolationsSingle dfa wfs
        e = observedViolationsSingle dfa salad * fromIntegral (totalWords wfs) / fromIntegral (totalWords salad)


generateGrammar :: forall g m clabel sigma . (RandomGen g, MonadState g m, Show clabel, Ix sigma, NFData sigma, NFData sigma, Eq clabel) -- In the typical case, m = StateT g IO, clabel = Glob, sigma = SegRef
    => (String -> m ()) -- function to log messages. (liftIO . putStrLn), write, and void are all good canduidates
    -> Int -- Monte Carlo sample size
    -> [Double] -- list of accuract thresholds
    -> [(clabel, SingleViolationCounter sigma)] -- list of constraints to try in order. Each constraint has a label and a dfa to compute it
    -> Lexicon sigma -- List of words to try and their frequencies
    -> m [(clabel, Double)] -- computed grammar
generateGrammar out samplesize thresholds (possibleConstraints) wfs = do
    let blankdfa = (nildfa . segBounds . snd . head $ possibleConstraints)
        lendist = lengthCdf wfs

        chooseConstraints :: Double -- accuracy
                          -> [clabel] -- grammar do far
                          -> MaxentViolationCounter sigma -- DFA of grammar so fat
                          -> Vec -- weights
                          -> Lexicon sigma -- word salad for evaluating expectations
                          -> [(clabel, SingleViolationCounter sigma)] -- candidates left
                          -> m ([clabel], MaxentViolationCounter sigma, Vec)
        chooseConstraints _ grammar dfa weights _ [] = do
            out "Pass complete."
            return (grammar, dfa, weights)
        chooseConstraints accuracy grammar dfa weights salad ((cl,cdfa):cs)
            | score > accuracy || cl `elem` grammar = chooseConstraints accuracy grammar dfa weights salad cs
            | otherwise = do
                out $ "Selected Constraint " ++ show cl ++  " (score " ++ showFFloat (Just 4) score ")."
                let newgrammar = cl:grammar
                    newdfa = dfaProduct consMC cdfa dfa
                out $ "New grammar has " ++ (show . rangeSize . labelBounds $ newdfa) ++ " states."
                let oldweights = consVec 0 weights
                    newweights = llpOptimizeWeights wfs newdfa oldweights
                out $ "Recalculated weights: " ++ showFVec (Just 2) newweights
                newsalad' <- fmap force $ sampleWordSalad (dropCounts (weightConstraints newdfa newweights)) lendist samplesize
                let newsalad = sortLexicon . fmap (\x -> (x,1)) $ newsalad'
                -- out $ "Generated new salad."
                chooseConstraints accuracy newgrammar newdfa newweights newsalad cs
            where
                score = evaluateConstraint wfs salad cdfa

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
            let salad = sortLexicon . fmap (\x -> (x,1)) $ salad'
            (newgrammar, newdfa, newweights) <- chooseConstraints acc grammar dfa weights salad possibleConstraints
            accuracyPass newgrammar newdfa newweights accs

    -- actually call our recursive function
    (grammar, dfa, weights) <- accuracyPass [] blankdfa zero thresholds

    return . reverse $ zip grammar (coords weights)

generateGrammarIO :: (Ix sigma, Show clabel, Eq clabel, NFData sigma) => Int -- Monte Carlo sample size
    -> [Double] -- list of accuract thresholds
    -> [(clabel, SingleViolationCounter sigma)] -- list of constraints to try in order. Each constraint has a label and a dfa to compute it
    -> Lexicon sigma -- List of words to try and their frequencies
    -> Int --random seed
    -> IO [(clabel, Double)]
generateGrammarIO samplesize thresholds (possibleConstraints) wfs seed =
    evalStateT (generateGrammar (liftIO . putStrLn) samplesize thresholds (possibleConstraints) wfs) (mkStdGen seed)
