{-# LANGUAGE ScopedTypeVariables, ExplicitForAll, BangPatterns #-}

{-|
Module: Linguistics.PhonotacticLearner.Util.ConjugateGradient
Description: Line search and Conjugate Gradient Search.
Copyright: © 2016-2017 George Steel and Peter Jurgec
License: GPL-2+
Maintainer: george.steel@gmail.com

Implementations of line search and conjugate gradient search for minimization. Line search uses Illinois False Position.
-}

module Linguistics.PhonotacticLearner.Util.ConjugateGradient (
    traceInline, regulaFalsiSearch, conjugateGradientSearch,

    -- llpOptimizeWeights
) where

import qualified Data.Map as M
import Data.List
import Data.Ix
import Debug.Trace
import qualified Data.Vector.Unboxed as V
import System.IO
import System.IO.Unsafe
import Numeric
import Data.Array.IArray
--import Linguistics.PhonotacticLearner.WeightedDFA
--import Linguistics.PhonotacticLearner.Util.Probability
import Linguistics.PhonotacticLearner.Util.Ring
--import Linguistics.PhonotacticLearner.MaxentGrammar

-- using conjugate gradient method ad described by Shewchuk in
-- "An Introduction to the Conjugate Gradient Method Without the Agonizing Pain"


-- length of starting guess for line search
rfInitSigma :: Double
rfInitSigma = 0.05

-- | Version of 'trace' which does not output a trailing linebreak. Good for progress bars.
traceInline :: String -> a -> a
traceInline s x = unsafePerformIO $ do
    hPutStr stderr s
    hFlush stderr
    return x

-- | Line search minimization using a modified Illinois False Position method.
--
-- Adapted from description at https://en.wikipedia.org/wiki/False_position_method
regulaFalsiSearch :: Double -- ^ stoping threshold uncertainty
                  -> (Vec -> Vec -> Double) -- ^ derivative of function to minimize
                  -> Vec -- ^ starting point
                  -> Vec -- ^ direction to search in
                  -> Vec -- ^ minimum point
regulaFalsiSearch epsilon f' xinit sdir = if (dxinit > 0) then xinit else pos (rfs a1 a2 0)
    where
        dir = normalizeVec sdir
        dxinit = f' xinit dir
        pos :: Double -> Vec
        pos alpha = xinit ⊕ (alpha ⊙ dir)
        doublingSearch = [(a, f' (pos a) dir) | a <- iterate (*2) rfInitSigma]
        (a1,a2) = head (filter (\((_,dx),(_,dy)) -> (dx <= 0) && (dy >= 0)) (zip ((0, dxinit):doublingSearch) doublingSearch))
        secant (!x,!dx) (!y,!dy) = (x*dy - y*dx) / (dy - dx)
        rfs :: (Double, Double) -> (Double, Double) -> Int -> Double
        rfs (!x,!dx) (!y,!dy) !bal
            | (dx == 0) = x
            | (dy == 0) = y
            | ((y-x) < epsilon) = secant (x,dx) (y,dy)
            | (dz <= 0) = {-traceShow (x,y, dx, dy, bal) $-} rfs (z,dz) (y,dy) (min bal 0 - 1)
            | otherwise = {-traceShow (x,y, dx, dy, bal) $-} rfs (x,dx) (z,dz) (max bal 0 + 1)
            where
                sy = if bal <= (-2) then (0.707 ^ negate bal) else 1
                sx = if bal >= 2 then (0.707 ^ bal) else 1
                z = (secant (x, sx*dx) (y, sy*dy))
                dz = f' (pos z) dir

-- | Nonlinear conjugate gradient search using Polak-Ribière method.
-- Stopping condition is two steps both havong a delta below the threshold.
conjugateGradientSearch :: Bool -- ^ trace progress to 'stderr' if true
                        -> (Double, Double) -- ^ stopping thresholds for conjugate gradient step and line search
                        -> (Vec -> (Vec, Bool)) -- ^ function to project points back into area defined by inequality constraints
                                                -- (for unconstrained problems use @(\x->(x,False))@)
                        -> (Vec -> (Double, Vec)) -- ^ function to minimize, returns value and gradient
                        -> (Vec -> Vec -> Double) -- ^ partial derivative of function to minimize
                        -> Vec -- ^ starting point
                        -> Vec -- ^ minimum point
conjugateGradientSearch shouldtrace (e1, e2) conproj fstar f' start = cjs dims (start ⊕ vec [2*e1]) zero zero start
    where                                       -- fake last step triggers restart and aviods stopping condition
        opttrace = if shouldtrace then traceInline else const id
        dims = length (coords start)
        cjs :: Int -> Vec -> Vec -> Vec -> Vec -> Vec
        cjs !bal !oldx !olddir !oldgrad !x = if normVec (oldx ⊖ x) < e1 || normVec (x ⊖ newx) < e1 -- two steps small enough
                                             then newx
                                             else cjs nbal' x sdir grad newx'
            where
                (v,grad) = fstar x
                beta' = innerProd grad (grad ⊖ oldgrad) / innerProd oldgrad oldgrad --Polak-Ribière
                (beta, nbal) = if (bal >= dims || beta' <= 0) then (0,0) else (beta', bal + 1)
                sdir = (beta ⊙ olddir) ⊖ grad
                newx = opttrace (if beta <= 0 then "+" else "-") $ regulaFalsiSearch e2 f' x sdir
                (newx', iscorr) = conproj newx
                nbal' = if iscorr then dims else nbal
