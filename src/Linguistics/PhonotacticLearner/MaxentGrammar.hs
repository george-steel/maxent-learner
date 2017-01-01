{-# LANGUAGE ScopedTypeVariables,
             ExplicitForAll,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances,
             BangPatterns #-}

module Linguistics.PhonotacticLearner.MaxentGrammar where

import Linguistics.PhonotacticLearner.Util.Ring
import Linguistics.PhonotacticLearner.WeightedDFA
import Linguistics.PhonotacticLearner.Util.Probability

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

-- aliases for various roles of transducer
type SingleViolationCounter sigma = DFST Int sigma (Sum Int)
type MaxentViolationCounter sigma = DFST Int sigma Multicount
type MaxentProbTransducer sigma = DFST Int sigma Double
type MaxentExpTransducer sigma = DFST Int sigma (Expectation Vec)


-- given maxent weights, turn count into their expectations
weightMC :: Vec -> Multicount -> Expectation Vec
weightMC weights counts = Exp p (p ⊙ fromMC counts)
    where p = exp . negate $ innerProd weights (fromMC counts)

-- add weights to a counting transducer to get expectation transducer
weightConstraints :: (Ix sigma) => MaxentViolationCounter sigma -> Vec -> MaxentExpTransducer sigma
weightConstraints dfa ws = fmap (weightMC ws) dfa

-- drop expectation data to give probability-only transducer
dropCounts :: (Ix sigma) => MaxentExpTransducer sigma -> MaxentProbTransducer sigma
dropCounts = fmap prob

-- get list total maxent values by string length. corecursive.
-- each entry in the list gives the total probability of all strings fo length n
-- Hayes and Wilson refer to this quantity as Z
maxentTotals :: (Ix sigma) => MaxentProbTransducer sigma -> [Double]
maxentTotals dfa = fmap totalp (iterate (stepweights dfa) (initialWeightArray dfa))
    where totalp arr = sum $ zipWith (*) (elems (finalWeights dfa)) (elems arr)

-- get expectation vector of gonstraints over the probability distribution defined by the transducer
-- returns a colist of vectors for each length of string
expectedViolations :: (RingModule Double v, Ix sigma) => DFST Int sigma (Expectation v) -> [(Double, v)]
expectedViolations dfa = fmap totalexp (iterate (stepweights dfa) (initialWeightArray dfa))
    where totalexp arr = (prob &&& normalizeExp) . sumR $ zipWith (⊗) (elems (finalWeights dfa)) (elems arr)



type Length = Int

-- list of words with their frequenceis sorted by length
data Lexicon sigma = Lex { totalWords :: Int
                         , lengthFreqs :: Array Length Int
                         , wordFreqs :: [([sigma], Int)]
                         } deriving Show

-- convert jumbled list fo words and frequencies to sorted lexicon
sortLexicon :: (Ord sigma) => [([sigma],Int)] -> Lexicon sigma
sortLexicon wfs = Lex twords alengths awf
    where
        mwf = M.fromListWith (+) wfs
        awf = M.assocs mwf
        mlengths = M.mapKeysWith (+) length mwf
        maxlen = fst (M.findMax mlengths)
        alengths = accumArray (+) 0 (0,maxlen) (M.assocs mlengths)
        twords = sum (M.elems mlengths)

lengthCdf :: Lexicon sigma -> Cdf Length
lengthCdf = massToCdf . assocs . fmap fromIntegral . lengthFreqs

lengthPdf :: Lexicon sigma -> [(Length, Double)]
lengthPdf wfs = assocs . fmap fracOfTotal . lengthFreqs $ wfs
    where fracOfTotal k = fromIntegral k / fromIntegral (totalWords wfs)


-- get log probability of lexicon given total violations (taken as a parameter for caching), a violation counter, and a set of weights.
lexLogProb :: (Ix sigma) => Lexicon sigma -> Vec -> MaxentViolationCounter sigma -> Vec -> Double
lexLogProb wfs oviols ctr weights = totalViolWeight + totalNormalizer + prior
    where
        --prior = innerProd weights weights / 2
        prior = l1Vec weights
        probdfa = dropCounts (weightConstraints ctr weights)
        probs = maxentTotals probdfa
        totalViolWeight = innerProd oviols weights
        totalNormalizer = sum . fmap (\(l,n) -> n ⊙ log (probs !! l)) . assocs . lengthFreqs $ wfs

priorDeriv :: Vec -> Vec -> Vec
priorDeriv (Vec !weights) (Vec !dlp) = Vec $ V.zipWith (\w d -> if w < 0.01 then min (d+1) 0 else d+1) weights dlp
{-# INLINE priorDeriv #-}

-- same as logProb, but also returns its derivative at the weight vector specified.
lexLogProbTotalDeriv :: (Ix sigma) => Array Length Int -> Vec -> MulticountDFST sigma -> Vec -> (Double, Vec)
lexLogProbTotalDeriv !lengths !oviols !ctr !weights = (totalViolWeight + totalNormalizer + prior, priorDeriv weights (oviols ⊖ expviols))
    where
        --prior = innerProd weights weights / 2
        prior = l1Vec weights
        edfa = weightExpVec ctr weights
        (_,maxlen) = bounds lengths
        exps = expsByLengthVec edfa maxlen
        totalViolWeight = innerProd oviols weights
        totalNormalizer = sum . fmap (\(l,n) -> n ⊙ log (prob (exps ! l))) . assocs $ lengths
        expviols = sumR . fmap (\(l,n) -> n ⊙ normalizeExp (exps ! l)) . assocs $ lengths

dotWeights :: Vec -> Expectation Vec-> Expectation Double
dotWeights ws (Exp p es) = Exp p (innerProd es ws)

lexLogProbPartialDeriv :: (Ix sigma) => Array Length Int -> Vec -> MulticountDFST sigma -> Vec -> Vec -> Double
lexLogProbPartialDeriv lengths oviols ctr weights dir = innerProd (dl1Vec weights) dir + innerProd dir oviols - expviols
    where
        edfa = weightExpPartial ctr weights dir
        (_,maxlen) = bounds lengths
        exps = expsByLengthDouble edfa maxlen
        expviols = sumR . fmap (\(l,n) -> n ⊙ normalizeExp (exps ! l)) . assocs $ lengths






-- returns a monadic action to sample random words from a probability transducer.
-- for efficiency, evaluate this once than sequence the action repeatedly as intermediate values will be memoized.
sampleWord :: forall g sigma m . (RandomGen g, Ix sigma, MonadState g m) => MaxentProbTransducer sigma -> Int -> (Int -> m [sigma])
sampleWord dfa maxn = backDists `seq` \n -> do
        fs <- sampleCdf (finalStates ! n)
        rcs <- flip evalStateT fs . forM (reverse . range $ (1,n)) $ \k -> do
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

sampleWordSalad :: (RandomGen g, Ix sigma, MonadState g m) => MaxentProbTransducer sigma -> Cdf Int -> Int -> m [[sigma]]
sampleWordSalad dfa lengthdist samples = mapM sampler wordlenlist
    where
        wordlen = uniformSample lengthdist samples
        maxn = maximum (fmap fst wordlen)
        sampler = sampleWord dfa maxn
        wordlenlist = wordlen >>= uncurry (flip replicate)
