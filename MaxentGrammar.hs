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
import qualified Data.Map as M
--------------------------------------------------------------------------------
-- Expectation semirings for transducers

addThese :: (Num a) => These a a -> a
addThese (This a) = a
addThese (That b) = b
addThese (These a b) = a + b

addAlign :: (Num a) => [a] -> [a] -> [a]
addAlign = alignWith addThese

newtype Multicount = MC [Int] deriving (Eq, Show)

instance Monoid Multicount where
    mempty = MC []
    mappend (MC x) (MC y) = MC (addAlign x y)

consMC :: Int -> Multicount -> Multicount
consMC x (MC xs) = MC (x : xs)

singleMC :: Int -> Multicount
singleMC x = MC [x]

data Expectation = Exp { prob :: Double
                       , exps :: [Double] }
                   deriving (Eq, Show)

instance Semiring Expectation where
    zero = Exp 0 []
    one = Exp 1 []
    (Exp p1 v1) <+> (Exp p2 v2) = Exp (p1 + p2) (addAlign v1 v2)
    (Exp p1 v1) <.> (Exp p2 v2) = Exp (p1 * p2) (addAlign (fmap (p1 *) v2) (fmap (p2 *) v1))


weightMC :: [Double] -> Multicount -> Expectation
weightMC weights (MC counts) = Exp p (fmap (\x -> p * fromIntegral x) counts)
    where p = product (zipWith (\w c -> exp . negate . (p*) . fromIntegral $ c) weights counts)

normalizeExp :: Expectation -> [Double]
normalizeExp (Exp p vs) = fmap (/ p) vs

--------------------------------------------------------------------------------

type MaxentViolationCounter sigma = WDFA Int sigma Multicount
type MaxentProbTransducer sigma = WDFA Int sigma Double
type MaxentExpTransducer sigma = WDFA Int sigma Expectation

weightConstraints :: (Ix sigma) => MaxentViolationCounter sigma -> [Double] -> MaxentExpTransducer sigma
weightConstraints dfa ws = mapweights (weightMC ws) dfa

dropCounts :: (Ix sigma) => MaxentExpTransducer sigma -> MaxentProbTransducer sigma
dropCounts = mapweights prob

expectedViolations :: (Ix sigma) => MaxentExpTransducer sigma -> [[Double]]
expectedViolations dfa = fmap (normalizeExp . sumR) (iterate (stepweights dfa) (initialWeightArray dfa))



--------------------------------------------------------------------------------

newtype Cdf a = Cdf (M.Map Double a)

massToCdf :: [(a, Double)] -> Cdf a
massToCdf xs = Cdf (M.fromList (zip partialsums (fmap fst xs')))
    where
        xs' = filter ((/=0) . snd) xs
        totalp = sum (fmap snd xs)
        partialsums = scanl (+) 0 (fmap snd xs')


samplecdf :: (RandomGen g, MonadState g m) => Cdf a -> m a
samplecdf (Cdf cdm) = do
    y :: Double <- state (randomR (0,1))
    let (Just (_,x)) = M.lookupLE y cdm
    return x
    

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

