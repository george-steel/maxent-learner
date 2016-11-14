{-# LANGUAGE ScopedTypeVariables, ExplicitForAll, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Probability where

import Ring
--import Data.List
import Data.Tuple
import Data.Monoid
import Data.Align
import qualified Data.Map as M
import System.Random
import Control.Monad.State

-- monoid for counting multiple quantities.
-- Union of the lattices Z < Z^2 < Z^3 < ...
newtype Multicount = MC {getMC :: [Int]} deriving (Eq, Show)

instance Monoid Multicount where
    mempty = MC []
    mappend (MC x) (MC y) = MC (alignWith addThese x y)

-- add new count to head of list
consMC :: Sum Int -> Multicount -> Multicount
consMC (Sum x) (MC xs) = MC (x : xs)

-- convert single counter to multicount
singleMC :: Sum Int -> Multicount
singleMC (Sum x) = MC [x]

fromMC :: Multicount -> Vec
fromMC = Vec . fmap fromIntegral . getMC


--------------------------------------------------------------------------------

-- Expectation semirings for transducers

-- expectation semiring as described by Eisner
-- holds total probability of event and the events contribution to an expectation vector
data Expectation v = Exp { prob :: Double
                         , exps :: v }
                   deriving (Eq, Show)

-- combine exclusive events
instance (RingModule Double v) => Additive (Expectation v) where
    zero = Exp 0 zero
    (Exp p1 v1) ⊕ (Exp p2 v2) = Exp (p1 + p2) (v1 ⊕ v2)

-- intersect independent events or combine event with conditional probability
instance (RingModule Double v) => Semiring (Expectation v) where
    one = Exp 1 zero
    (Exp p1 v1) ⊗ (Exp p2 v2) = Exp (p1 * p2) ((p1 ⊙ v2) ⊕ (p2 ⊙ v1))

-- expectation conditional on the event reperesented occuring
normalizeExp :: (RingModule Double v) => Expectation v -> v
normalizeExp (Exp p vs) = (1/p) ⊙ vs


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

uniformSample :: Cdf a -> Int -> [(a, Int)]
uniformSample (Cdf cmf) n = zipWith subentries (tail breaks ++ [(undefined, n)]) breaks
    where
        breaks = fmap (fmap (round . (n ⊙)) . swap) . M.assocs $ cmf
        subentries (_,cx) (y,cy) = (y,cx-cy)

--------------------------------------------------------------------------------

upperConfidenceOE :: Double -> Double -> Double
upperConfidenceOE o e = if p >= 1 then 1 else min 1 (p + 3*v)
    where
        p = (o + 0.5) / (e+1)
        v = sqrt (p * (1-p) / (e+1))
