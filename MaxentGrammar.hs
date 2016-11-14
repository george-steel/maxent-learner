{-# LANGUAGE ScopedTypeVariables, ExplicitForAll, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module MaxentGrammar where

import Ring
import WeightedDFA
import Probability
import Data.Align
import Data.These
import Data.Array.IArray
import Data.Array.Unboxed
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Class
import Control.Arrow
import System.Random
import Data.List
import Data.Monoid
import Data.Tuple
import qualified Data.Map as M
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------

-- aliases for various roles of transducer
type SingleViolationCounter sigma = WDFA Int sigma (Sum Int)
type MaxentViolationCounter sigma = WDFA Int sigma Multicount
type MaxentProbTransducer sigma = WDFA Int sigma Double
type MaxentExpTransducer sigma = WDFA Int sigma (Expectation Vec)


-- given maxent weights, turn count into their expectations
weightMC :: Vec -> Multicount -> Expectation Vec
weightMC weights counts = Exp p (p ⊙ fromMC counts)
    where p = exp . negate $ innerProd weights (fromMC counts)

-- add weights to a counting transducer to get expectation transducer
weightConstraints :: (Ix sigma) => MaxentViolationCounter sigma -> Vec -> MaxentExpTransducer sigma
weightConstraints dfa ws = mapweights (weightMC ws) dfa

-- drop expectation data to give probability-only transducer
dropCounts :: (Ix sigma) => MaxentExpTransducer sigma -> MaxentProbTransducer sigma
dropCounts = mapweights prob

-- get list total maxent values by string length. corecursive.
-- each entry in the list gives the total probability of all strings fo length n
-- Hayes and Wilson refer to this quantity as Z
maxentTotals :: (Ix sigma) => MaxentProbTransducer sigma -> [Double]
maxentTotals dfa = fmap sum (iterate (stepweights dfa) (initialWeightArray dfa))

-- get expectation vector of gonstraints over the probability distribution defined by the transducer
-- returns a corecursive list of vectors for each length of string
expectedViolations :: (RingModule Double v, Ix sigma) => WDFA Int sigma (Expectation v) -> [(Double, v)]
expectedViolations dfa = fmap ((prob &&& normalizeExp) . sumR) (iterate (stepweights dfa) (initialWeightArray dfa))




-- list of words with their frequenceis sorted by length
data Lexicon sigma = Lex { totalOccurrences :: Int
                         , wordsByLength :: M.Map Int (Int, M.Map [sigma] Int)
                         } deriving Show

-- convert jumbled list fo words and frequencies to sorted lexicon
sortLexicon :: (Ord sigma) => [([sigma],Int)] -> Lexicon sigma
sortLexicon wfs = Lex (sum . fmap fst $ mwf) mwf
    where mwf = M.unionsWith (\(nx,xs) (ny,ys) -> (nx+ny, M.unionWith (+) xs ys)) [length w =: (f, w =: f) | (w,f) <- wfs]

lengthDistribution :: Lexicon sigma -> Cdf Int
lengthDistribution = massToCdf . M.assocs . fmap (fromIntegral . fst) . wordsByLength


(=:) :: k -> a -> M.Map k a
(=:) = M.singleton



-- count totasl violations of each constraint and return totals (and the total number of words) for each length
observedViolations :: (Ix sigma) => MaxentViolationCounter sigma -> Lexicon sigma -> M.Map Int (Int, [Int])
observedViolations dfa (Lex _ wfs) = fmap totalviols wfs
    where
        viols (w,n) =  fmap (n *) . getMC . transduce dfa $ w
        totalviols (nw, ws) = (nw, (fmap sum . transpose . fmap viols . M.assocs $ ws))



-- get log probability of lexicon given total violations (taken as a parameter for caching), a violation counter, and a set of weights.
lexLogProb :: (Ix sigma) =>  M.Map Int (Int, [Int]) -> MaxentViolationCounter sigma -> Vec -> Double
lexLogProb viols ctr weights = sum . fmap logProb . M.assocs $ viols
    where
        pdfa = dropCounts (weightConstraints ctr weights)
        probs = fmap log (maxentTotals pdfa)
        logProb :: (Int, (Int, [Int])) -> Double
        logProb (k, (n, vs)) = (n ⊙ (probs !! k)) + innerProd (fromInts vs) weights

-- same as logProb, but also returns its derivative at the weight vector specified.
lexLogProbTotalDeriv :: (Ix sigma) =>  M.Map Int (Int, [Int]) -> MaxentViolationCounter sigma -> Vec -> (Double, Vec)
lexLogProbTotalDeriv viols ctr weights = (sum (fmap logProb (M.assocs viols)), (1 / nwords) ⊙ sumR (fmap dLogProb (M.assocs viols)))
    where
        nwords :: Double = fromIntegral (sum (fmap fst viols))
        edfa = weightConstraints ctr weights
        exps = expectedViolations edfa
        logProb :: (Int, (Int, [Int])) -> Double
        logProb (k, (n, vs)) = (n ⊙ log (fst (exps !! k))) + innerProd (fromInts vs) weights
        dLogProb :: (Int, (Int, [Int])) -> Vec
        dLogProb (k, (n, vs)) = fromInts vs ⊖ (n ⊙ snd (exps !! k))

dotWeights :: Vec -> Expectation Vec-> Expectation Double
dotWeights ws (Exp p es) = Exp p (innerProd es ws)

lexLogProbPartialDeriv :: (Ix sigma) => M.Map Int (Int, [Int]) -> MaxentViolationCounter sigma -> Vec -> Vec -> Double
lexLogProbPartialDeriv viols ctr weights dir = (sum . fmap dLogProb . M.assocs $ viols) / nwords
    where
        nwords = fromIntegral (sum (fmap fst viols))
        edfa = mapweights (dotWeights dir) (weightConstraints ctr weights)
        exps = expectedViolations edfa
        dLogProb :: (Int, (Int, [Int])) -> Double
        dLogProb (k, (n, vs)) = innerProd dir (fromInts vs) - (n ⊙ snd (exps !! k))






-- returns a monadic action to sample random words from a probability transducer.
-- for efficiency, evaluate this once than sequence the action repeatedly as intermediate values will be memoized.
sampleWord :: forall g sigma m . (RandomGen g, Ix sigma, MonadState g m) => MaxentProbTransducer sigma -> Int -> (Int -> m [sigma])
sampleWord dfa maxn = backDists `seq` \n -> do
        fs <- samplecdf (finalStates !n)
        rcs <- flip evalStateT fs . forM (reverse . range $ (1,n)) $ \k -> do
            s <- get
            (c,s') <- lift . samplecdf $ backDists!(k,s)
            put s'
            return c
        return (reverse rcs)
    where
        backnfa = reverseDFA dfa
        (smin, smax) = labelBounds dfa

        maxentPrefixes = take (maxn + 1) (iterate (stepweights dfa) (initialWeightArray dfa))
        maxentArray :: UArray (Int,Int) Double
        maxentArray = array ((0,smin), (maxn,smax)) . join . snd . mapAccumL (\k a -> (k+1, fmap (\(x,p)->((k,x),p)) (assocs a))) 0 $ maxentPrefixes

        backDist :: (Int, Int) -> Cdf (sigma, Int)
        backDist (k, s) = massToCdf $ do
            c <- range (segBounds dfa)
            (s', w) <- backnfa!(s,c)
            return ((c,s'), w * (maxentArray!(k-1,s')))
        -- memoized version
        backDists :: Array (Int, Int) (Cdf (sigma, Int))
        backDists = array ((1,smin), (maxn,smax)) (fmap (id &&& backDist) (range ((1,smin), (maxn,smax))))

        finalStates :: Array Int (Cdf Int)
        finalStates = array (1,maxn) $ do
            n <- range (1,maxn)
            let cdf = massToCdf $ do
                    s <- range (smin,smax)
                    return (s, maxentArray!(n,s))
            return (n,cdf)

sampleWordSalad :: (RandomGen g, Ix sigma, MonadState g m) => MaxentProbTransducer sigma -> Cdf Int -> Int -> m [[sigma]]
sampleWordSalad dfa lengthdist samples = mapM sampler wordlenlist
    where
        wordlen = uniformSample lengthdist samples
        maxn = maximum (fmap fst wordlen)
        sampler = sampleWord dfa maxn
        wordlenlist = wordlen >>= uncurry (flip replicate)
        
