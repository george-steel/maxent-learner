{-# LANGUAGE ScopedTypeVariables,
             ExplicitForAll,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances,
             BangPatterns #-}

{-|
Module: Linguistics.PhonotacticLearner.MaxentGrammar.MaxentGrammar
Description: Functions to model maxent grammars using DFSTs.
Copyright: © 2016-2017 George Steel and Peter Jurgec
License: GPL-2+
Maintainer: george.steel@gmail.com

Library for using DFAs to represent maxent grammars. A mexent grammar consists of a set of constraints, each of which is given a weight, which define a probability diatribution over the set of strings of each given length.
The relativve probability (maxent score) of each string is equal to the negative exponential of the the total weight of the violated constraints.  In this module, such a grammar is reperesented by a 'DFST' which can count violations and a 'Vec' of weights.

This module is mainly concerned with calculating probabilities of samples of text and finding the optimal weights to maximize that probability. There are alsu functions to randomly generate text using the distribution implied by a mexent grammar.
-}

module Linguistics.PhonotacticLearner.MaxentGrammar (
    Length, Lexicon(..), sortLexicon, lengthCdf, lengthPdf,

    maxentProb,
    lexLogProbTotalDeriv, lexLogProbPartialDeriv,
    llpOptimizeWeights,

    sampleWord, sampleWordSalad
) where

import Linguistics.PhonotacticLearner.Util.Ring
import Linguistics.PhonotacticLearner.WeightedDFA
import Linguistics.PhonotacticLearner.Util.Probability
import Linguistics.PhonotacticLearner.Util.ConjugateGradient

import Data.Array.IArray
import Data.Array.Unboxed
import Control.Monad
import Control.Monad.State
--import Control.Monad.Trans.Class
import Control.Arrow ((&&&),(***))
import System.Random
import Data.List as L
import Data.Monoid
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as V
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------

-- | Apply weights to violation counts to get a relative probability.
maxentProb :: Vec -> Multicount -> Double
maxentProb !weights !counts = exp . negate $ innerProd weights (fromMC counts)

{-# INLINE maxentProb #-}

type Length = Int

-- | Returns the probability (as a logarithm) of a lexicon with aand associated length distribution.
data Lexicon sigma = Lex { totalWords :: Int
                         , lengthFreqs :: Array Length Int
                         , wordFreqs :: [([sigma], Int)]
                         } deriving Show

-- | Convert jumbled list of words and frequencies to sorted lexicon.
sortLexicon :: (Ord sigma) => [([sigma],Int)] -> Lexicon sigma
sortLexicon wfs = Lex twords alengths awf
    where
        mwf = M.fromListWith (+) wfs
        awf = M.assocs mwf
        mlengths = M.mapKeysWith (+) length mwf
        maxlen = fst (M.findMax mlengths)
        alengths = accumArray (+) 0 (0,maxlen) (M.assocs mlengths)
        twords = sum (M.elems mlengths)

-- | Retrieve length distribution as a 'Cdf' for sampling.
lengthCdf :: Lexicon sigma -> Cdf Length
lengthCdf = massToCdf . assocs . fmap fromIntegral . lengthFreqs

-- | Retrieve length distribution as a normalized probability mass function. Probabilities add up to 1.
lengthPdf :: Lexicon sigma -> [(Length, Double)]
lengthPdf wfs = assocs . fmap fracOfTotal . lengthFreqs $ wfs
    where fracOfTotal k = fromIntegral k / fromIntegral (totalWords wfs)


priorDeriv :: Vec -> Vec -> Vec
priorDeriv (Vec !weights) (Vec !dlp) = Vec $ V.zipWith (\w d -> if w < 0.01 then min (d+1) 0 else d+1) weights dlp
{-# INLINE priorDeriv #-}

-- | For a given set of consteraints (reperesented by a DFST counting violations),
-- lexicon (reprersented as length distribution and total violation count, which should be precomputed),
-- and weight vector, returns the absolute probability (as a negative logarithm) and its derivative with respect to the weight vector.
--
-- Minimize this to find the optimal weights.
-- To prevent overfitting, this function includes an exponential (L₁) prior equivalent to each constraint being violated once for existing.
-- This intentionally differs from Hayes and Wilson since their gaussian (L₂²) prior had a strong preference for as many simillar constraints as possible as opposed to a single constraint. The exponential prior was chosen since it is independent of splitting constraints into duplicates with the weight distributed between them.
lexLogProbTotalDeriv :: (Ix sigma) => MulticountDFST sigma -- ^ DFST counting constraint violations
                                   -> Array Length Int -- ^ Length distribution of lexicon
                                   -> Vec -- ^ Observed violations in lexicon
                                   -> Vec -- ^ Weights to give constraints
                                   -> (Double, Vec) -- ^ Probability and its derivative w.r.t. the weights
lexLogProbTotalDeriv !ctr !lengths !oviols !weights = (totalViolWeight + totalNormalizer + prior, priorDeriv weights (oviols ⊖ expviols))
    where
        --prior = innerProd weights weights / 2
        prior = l1Vec weights
        edfa = weightExpVec ctr weights
        (_,maxlen) = bounds lengths
        exps = expsByLengthVec edfa maxlen
        totalViolWeight = innerProd oviols weights
        totalNormalizer = sum . fmap (\(l,n) -> n ⊙ log (prob (exps ! l))) . assocs $ lengths
        expviols = sumR . fmap (\(l,n) -> n ⊙ normalizeExp (exps ! l)) . assocs $ lengths


-- | Compute partial derivative of lexicon probability. Much faster equivalent of
--
-- > lexLogProbPartialDeriv ctr lengths oviols weights dir = dir `innerProd` snd (lexLogProbTotalDeriv ctr lengths oviols weights)

lexLogProbPartialDeriv :: (Ix sigma) => MulticountDFST sigma -> Array Length Int -> Vec -> Vec -> Vec -> Double
lexLogProbPartialDeriv !ctr !lengths !oviols !weights !dir = innerProd (dl1Vec weights) dir + innerProd dir oviols - expviols
    where
        edfa = weightExpPartial ctr weights dir
        (_,maxlen) = bounds lengths
        exps = expsByLengthDouble edfa maxlen
        expviols = sumR . fmap (\(l,n) -> n ⊙ normalizeExp (exps ! l)) . assocs $ lengths



zeroNeg :: Vec -> (Vec, Bool)
zeroNeg (Vec v) = (Vec (V.map (\x -> if x < 0.01 then 0 else x) v), V.any (\x -> x /= 0 && x < -0.01) v)

-- | Calculate weights to maximize probability of lexicon.
--  Takes starting position of search which MUST have the correct dimensionality (do not use 'zero')
llpOptimizeWeights :: (Ix sigma) => Array Length Int -- Length distribution
                                 -> PackedText sigma -- Packed lexicon
                                 -> MulticountDFST sigma -- Constraint violation counter
                                 -> Vec -- initial guess at weights
                                 -> Vec -- Weights
llpOptimizeWeights lengths pwfs dfa initweights =
    let oviols = fromMC (transducePackedMulti dfa pwfs)
    in conjugateGradientSearch True
                               (0.01, 0.005)
                               zeroNeg
                               (lexLogProbTotalDeriv dfa lengths oviols)
                               (lexLogProbPartialDeriv dfa lengths oviols)
                               initweights


--------------------------------------------------------------------------------


-- used for statistical calculations over an entire dfa with ring weights (e.g. probabilities)
-- given an array mapping states to weights (e.g, a maxent distribution),
-- gives a new distribution after transducing an additional character
stepweights :: (Ix q, Ix sigma, Semiring k) => DFST q sigma k -> Array q k -> Array q k
stepweights dfa@(DFST _ tm _) prev = accumArray (⊕) zero sbound (fmap pathweight (range (bounds tm)))
    where
        sbound = stateBounds dfa
        pathweight (s,c) = let (ns,w) = tm!(s,c) in (ns, (prev!s) ⊗ w)

-- gives an array from states to weights with 1 in the first position and 0 elsewhere
initialWeightArray :: (Ix l, Ix sigma, Semiring w) => DFST l sigma w -> Array l w
initialWeightArray dfa = fnArray (stateBounds dfa) (\x -> if x == initialState dfa then one else zero)

-- converts to an NFA with all the arrows reversed
reverseTM :: (Ix q, Ix sigma) => DFST q sigma k -> Array (q,sigma) [(q,k)]
reverseTM (DFST _ arr _) = accumArray (flip (:)) [] (bounds arr) (fmap (\((s,c),(s',w)) -> ((s',c),(s,w))) (assocs arr))


-- | Returns a monadic action to sample random words from a probability transducer,
-- which may be generated from a violation counter with @('fmap' ('maxentProb' weights) ctr)@).
-- For efficiency, evaluate this once then sequence the action repeatedly as intermediate values will be memoized.

sampleWord :: forall g sigma m . (RandomGen g, Ix sigma, MonadState g m)
    => DFST Int sigma Double -- ^ Probability DFST
    -> Length -- ^ Maximum length to greate generator fot
    -> (Length -> m [sigma]) -- ^ Random generator taking length and returning action.
sampleWord dfa maxn = backDists `seq` \n -> do
        fs <- sampleCdf (finalStates ! n)
        rcs <- flip evalStateT fs . forM (reverse . range $ (1, min n maxn)) $ \k -> do
            s <- get
            (c,s') <- lift . sampleCdf $ backDists!(k,s)
            put s'
            return c
        return (reverse rcs)
    where
        backnfa = reverseTM dfa
        qbound = stateBounds dfa

        maxentPrefixes = take (maxn + 1) (iterate (stepweights dfa) (initialWeightArray dfa))
        maxentArray :: UArray (Int,Int) Double
        maxentArray = array ((0,maxn) `xbd` qbound) . join . snd . mapAccumL (\k a -> (k+1, fmap (\(x,p)->((k,x),p)) (assocs a))) 0 $ maxentPrefixes

        backDist :: (Int, Int) -> Cdf (sigma, Int)
        backDist (k, s) = massToCdf $ do
            c <- range (segBounds dfa)
            (s', w) <- backnfa!(s,c)
            return ((c,s'), w * (maxentArray!(k-1,s')))
        -- memoized version
        backDists :: Array (Int, Int) (Cdf (sigma, Int))
        backDists = fnArray ((1,maxn) `xbd` qbound) backDist

        finalStates :: Array Int (Cdf Int)
        finalStates = array (1,maxn) $ do
            n <- range (1,maxn)
            let cdf = massToCdf $ do
                    s <- range qbound
                    return (s, maxentArray!(n,s) * finalWeights dfa!s)
            return (n,cdf)
-- | Like sampleWord but generates multiple words. Length distribution is specified as a 'Cdf' and number of words to generate.
sampleWordSalad :: (RandomGen g, Ix sigma, MonadState g m) => DFST Int sigma Double -> Cdf Length -> Int -> m [[sigma]]
sampleWordSalad dfa lengthdist samples = mapM sampler wordlenlist
    where
        wordlen = uniformSample lengthdist samples
        maxn = maximum (fmap fst wordlen)
        sampler = sampleWord dfa maxn
        wordlenlist = wordlen >>= uncurry (flip replicate)
