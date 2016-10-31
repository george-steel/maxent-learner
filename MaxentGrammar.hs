{-# LANGUAGE ScopedTypeVariables, ExplicitForAll #-}

module MaxentGrammar where

import Ring
import WeightedDFA
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
import qualified Data.Map as M
--------------------------------------------------------------------------------
-- Expectation semirings for transducers

(=:) :: k -> a -> M.Map k a
(=:) = M.singleton


addThese :: (Num a) => These a a -> a
addThese (This a) = a
addThese (That b) = b
addThese (These a b) = a + b

-- vector adition: add two lists as if they are followed by an infinite strings of list
-- output has length of longest input
addAlign :: (Num a) => [a] -> [a] -> [a]
addAlign = alignWith addThese

-- monoid for counting multiple quantities. 
-- Union of the lattices Z < Z^2 < Z^3 < ...
newtype Multicount = MC {getMC :: [Int]} deriving (Eq, Show)

instance Monoid Multicount where
    mempty = MC []
    mappend (MC x) (MC y) = MC (addAlign x y)

-- add new count to head of list
consMC :: Sum Int -> Multicount -> Multicount
consMC (Sum x) (MC xs) = MC (x : xs)

-- convert single counter to multicount
singleMC :: Sum Int -> Multicount
singleMC (Sum x) = MC [x]




-- expectation semiring as described by Eisner
-- holds total probability of event and the events contribution to an expectation vector
data Expectation = Exp { prob :: Double
                       , exps :: [Double] }
                   deriving (Eq, Show)

instance Semiring Expectation where
    zero = Exp 0 []
    one = Exp 1 []
    -- Add combine exclusive events
    (Exp p1 v1) <+> (Exp p2 v2) = Exp (p1 + p2) (addAlign v1 v2)
    -- intersect independent events or combine event with conditional probability
    (Exp p1 v1) <.> (Exp p2 v2) = Exp (p1 * p2) (addAlign (fmap (p1 *) v2) (fmap (p2 *) v1))

-- expectation conditional on the event reperesented occuring
normalizeExp :: Expectation -> [Double]
normalizeExp (Exp p vs) = fmap (/ p) vs

-- given maxent weights, turn count into their expectations
weightMC :: [Double] -> Multicount -> Expectation
weightMC weights (MC counts) = Exp p (fmap (\x -> p * fromIntegral x) counts)
    where p = product (zipWith (\w c -> exp . negate . (w*) . fromIntegral $ c) weights counts)


--------------------------------------------------------------------------------

-- aliases for various roles of transducer
type MaxentViolationCounter sigma = WDFA Int sigma Multicount
type MaxentProbTransducer sigma = WDFA Int sigma Double
type MaxentExpTransducer sigma = WDFA Int sigma Expectation


-- add weights to a counting transducer to get expectation transducer
weightConstraints :: (Ix sigma) => MaxentViolationCounter sigma -> [Double] -> MaxentExpTransducer sigma
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
expectedViolations :: (Ix sigma) => MaxentExpTransducer sigma -> [(Double, [Double])]
expectedViolations dfa = fmap ((prob &&& normalizeExp) . sumR) (iterate (stepweights dfa) (initialWeightArray dfa))




-- list of words with their frequenceis sorted by length
data Lexicon sigma = Lex { totalOccurrences :: Int
                         , wordsByLength :: M.Map Int (Int, M.Map [sigma] Int)
                         } deriving Show

-- convert jumbled list fo words and frequencies to sorted lexicon
sortLexicon :: (Ord sigma) => [([sigma],Int)] -> Lexicon sigma
sortLexicon wfs = Lex (sum . fmap fst $ mwf) mwf
    where mwf = M.unionsWith (\(nx,xs) (ny,ys) -> (nx+ny, M.unionWith (+) xs ys)) [length w =: (f, w =: f) | (w,f) <- wfs]

-- count totasl violations of each constraint and return totals (and the total number of words) for each length
observedViolations :: (Ord sigma, Ix sigma) => MaxentViolationCounter sigma -> Lexicon sigma -> M.Map Int (Int, [Int])
observedViolations dfa (Lex _ wfs) = fmap totalviols wfs
    where
        viols (w,n) =  n : (fmap (n *) . getMC . transduce dfa $ w)
        totalviols (nw, ws) = (nw, (fmap sum . transpose . fmap viols . M.assocs $ ws))

-- get log probability of lexicon given total violations (taken as a parameter for caching), a violation counter, and a set of weights.
lexLogProb :: (Ord sigma, Ix sigma) =>  M.Map Int (Int, [Int]) -> MaxentViolationCounter sigma -> [Double] -> Double
lexLogProb viols ctr weights = sum . fmap score . M.assocs $ viols
    where
        pdfa = dropCounts (weightConstraints ctr weights)
        probs = fmap log (maxentTotals pdfa)
        score (k, (n, vs)) = (fromIntegral n * (probs !! k)) + sum (zipWith (*) (fmap fromIntegral vs) weights)

-- same as logProb, but also returns its derivative at the weight vector specified.
lexLogProbDeriv :: (Ord sigma, Ix sigma) =>  M.Map Int (Int, [Int]) -> MaxentViolationCounter sigma -> [Double] -> (Double, [Double])
lexLogProbDeriv viols ctr weights = (sum . fmap logProb . M.assocs $ viols, fmap ((/ nwords) . sum) . transpose . fmap dLogProb . M.assocs $ viols)
    where
        nwords = fromIntegral (sum (fmap fst viols))
        edfa = weightConstraints ctr weights
        exps = expectedViolations edfa
        logProb :: (Int, (Int, [Int])) -> Double
        logProb (k, (n, vs)) = (fromIntegral n * log (fst (exps !! k))) + sum (zipWith (*) (fmap fromIntegral vs) weights)
        dLogProb :: (Int, (Int, [Int])) -> [Double]
        dLogProb (k, (n, vs)) = zipWith (-) (fmap fromIntegral vs) (fmap (fromIntegral n *) (snd (exps !! k)))

--------------------------------------------------------------------------------

-- cumulative probability distribution, can be sampled easily
newtype Cdf a = Cdf (M.Map Double a) deriving Show

-- comvert a list of probability or maxent masses to a Cdf
massToCdf :: [(a, Double)] -> Cdf a
massToCdf xs = Cdf (M.fromList (zip partialsums (fmap fst xs')))
    where
        xs' = filter ((/=0) . snd) xs
        totalp = sum (fmap snd xs)
        partialsums = scanl (+) 0 (fmap ((/ totalp) . snd) xs')

-- monadic action to sample a random value from a Cdf.
samplecdf :: (RandomGen g, MonadState g m) => Cdf a -> m a
samplecdf (Cdf cdm) = do
    y :: Double <- state (randomR (0,1))
    let (Just (_,x)) = M.lookupLE y cdm
    return x
    

-- returns a monadic action to sample random words from a probability transducer.
-- for efficiency, evaluate this once than sequence the action repeatedly as intermediate values will be memoized.
sampleWord :: forall g sigma m . (RandomGen g, Ix sigma, MonadState g m) => MaxentProbTransducer sigma -> Int -> m [sigma]
sampleWord dfa n = do
        fs <- samplecdf finalStates
        rcs <- flip evalStateT fs . forM (reverse . range $ (1,n)) $ \k -> do
            s <- get
            (c,s') <- lift . samplecdf $ backDists!(k,s)
            put s'
            return c
        return (reverse rcs)
    where
        backnfa = reverseDFA dfa
        (smin, smax) = labelBounds dfa
        
        maxentPrefixes = take (n + 1) (iterate (stepweights dfa) (initialWeightArray dfa))
        maxentArray :: UArray (Int,Int) Double 
        maxentArray = array ((0,smin), (n,smax)) . join . snd . mapAccumL (\n a -> (n+1, fmap (\(x,p)->((n,x),p)) (assocs a))) 0 $ maxentPrefixes
        
        backDist :: (Int, Int) -> Cdf (sigma, Int)
        backDist (k, s) = massToCdf $ do
            c <- range (segBounds dfa)
            (s', w) <- backnfa!(s,c)
            return ((c,s'), w * (maxentArray!(n-1,s')))
        -- memoized version
        backDists :: Array (Int, Int) (Cdf (sigma, Int))
        backDists = array ((1,smin), (n,smax)) (fmap (id &&& backDist) (range ((1,smin), (n,smax))))
        
        finalStates = massToCdf $ do
            s <- range (smin,smax)
            return (s, maxentArray!(n,s))

