{-# LANGUAGE ScopedTypeVariables, ExplicitForAll, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, GeneralizedNewtypeDeriving #-}

{-|
Module: Linguistics.PhonotacticLearner.Util.Probability
Description: Data structures and functions for working with probabilities.
Copyright: © 2016-2017 George Steel and Peter Jurgec
License: GPL-2+
Maintainer: george.steel@gmail.com

Data structures and functions for counting and probability.
-}

module Linguistics.PhonotacticLearner.Util.Probability (
    -- * Counting
    Multicount (..),
    getCounts, consMC, singleMC, fromMC,
    -- * Expectations
    Expectation(..),
    normalizeExp,
    -- * Sampling and Distributions
    Cdf, massToCdf, sampleCdf, uniformSample,
    upperConfidenceOE
) where

import Linguistics.PhonotacticLearner.Util.Ring
--import Data.List
import Data.Tuple
import Data.Monoid
import qualified Data.Map as M
import System.Random
import Control.Monad.State
import qualified Data.Vector.Unboxed as V
import Control.DeepSeq

-- | Monoid holding a list of integer counters which are summed independently
newtype Multicount = MC {unMC :: V.Vector Int} deriving (Eq, Show, NFData)

-- | Return the counts as a list of 'Int's
getCounts :: Multicount -> [Int]
getCounts (MC xs) = V.toList xs

instance Monoid Multicount where
    mempty = MC V.empty
    mappend (MC xs) (MC ys)
        | V.null xs = MC ys
        | V.null ys = MC xs
        | lx == ly = MC (V.zipWith (+) xs ys)
        | lx < ly = MC (V.zipWith (+) xs (V.take lx ys) V.++ V.drop lx ys)
        | ly < lx = MC (V.zipWith (+) ys (V.take ly xs) V.++ V.drop ly xs)
        where lx = V.length xs
              ly = V.length ys

-- | Add a new count to the head of the list
consMC :: Sum Int -> Multicount -> Multicount
consMC (Sum x) (MC xs) = MC (V.cons x xs)

-- | Use a single coutner as a Multicount.
singleMC :: Sum Int -> Multicount
singleMC (Sum x) = MC (V.singleton x)

-- | Convert the counts to coordinates
fromMC :: Multicount -> Vec
fromMC (MC xs) = Vec (V.map fromIntegral xs)


--------------------------------------------------------------------------------

{-| Expectation semiring as described by Eisner.

Represents an events contribution to the total expectation of a vector-valued variable. Addition takes the union of mutually exclusive events and multiplication either takes the intersection fo independent events or applies a conditional probability.

As a simple example, the expectation of the total value from rolling a 2 on a 6 sided die would be @Exp (1/6) (2/6)@.
-}
data Expectation v = Exp {
      prob :: {-# UNPACK #-} !Double -- ^ Probability of event occuring.
    , exps :: !v -- ^ Event's contribution to expectation of the variable
} deriving (Eq, Show)

-- combine exclusive events
instance (RingModule Double v) => Additive (Expectation v) where
    zero = Exp 0 zero
    (Exp p1 v1) ⊕ (Exp p2 v2) = Exp (p1 + p2) (v1 ⊕ v2)

-- intersect independent events or combine event with conditional probability
instance (RingModule Double v) => Semiring (Expectation v) where
    one = Exp 1 zero
    (Exp p1 v1) ⊗ (Exp p2 v2) = Exp (p1 * p2) ((p1 ⊙ v2) ⊕ (p2 ⊙ v1))

-- | Get the expectation conditional on the event actually occurring.
normalizeExp :: (RingModule Double v) => Expectation v -> v
normalizeExp (Exp p vs) = (1/p) ⊙ vs


--------------------------------------------------------------------------------

-- | Cumulative distribution table that can be sampled easily.
newtype Cdf a = Cdf (M.Map Double a) deriving Show

-- | Generate a CDF which from a list of outcomes and their relative probabilities (their sum will eb normalized and does not have to be 1).
massToCdf :: [(a, Double)] -> Cdf a
massToCdf xs = Cdf (M.fromList (zip partialsums (fmap fst xs')))
    where
        xs' = filter ((/=0) . snd) xs
        totalp = sum (fmap snd xs)
        partialsums = scanl (+) 0 (fmap ((/ totalp) . snd) xs')

-- | Sample a random variable according to a 'Cdf', gets the random generator state from the monad.
sampleCdf :: (RandomGen g, MonadState g m) => Cdf a -> m a
sampleCdf (Cdf cdm) = do
    y :: Double <- state (randomR (0,1))
    let (Just (_,x)) = M.lookupLE y cdm
    return x

-- | Deterministically sample n points spaced throughout the distribution. Used when the number of samples greatly outnumbers the number of outcomes.
uniformSample :: Cdf a -> Int -> [(a, Int)]
uniformSample (Cdf cmf) n = zipWith subentries (tail breaks ++ [(undefined, n)]) breaks
    where
        breaks = fmap (fmap (round . (n ⊙)) . swap) . M.assocs $ cmf
        subentries (_,cx) (y,cy) = (y,cx-cy)

--------------------------------------------------------------------------------

-- | Get the upper confidence bound of Observed/Expected
upperConfidenceOE :: Double -> Double -> Double
upperConfidenceOE o e = if p >= 1 then 1 else min 1 (p + 3*v)
    where
        p = (o + 0.5) / (e+1)
        v = sqrt (p * (1-p) / (e+1))
