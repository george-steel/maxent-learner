{-# LANGUAGE ScopedTypeVariables, ExplicitForAll #-}
module WeightOptimizer where

import Ring
import MaxentGrammar
import Data.List
import qualified Data.Map as M
import Data.Ix
import Debug.Trace

-- using conjugate gradient method ad described by Shewchuk in
-- "An Introduction to the Conjugate Gradient Method Without the Agonizing Pain"


-- tuning parameters for numerical approximation
rfInitSigma :: Double
rfInitSigma = 0.05


-- line search minimization using Illinois False Position method
-- algorithm adapted from description at
-- https://en.wikipedia.org/wiki/False_position_method
regulaFalsiSearch :: Double -> (Vec -> Vec -> Double) -> Vec -> Vec -> Vec
regulaFalsiSearch epsilon f' xinit sdir = if (dxinit > 0) then xinit else pos (rfs a1 a2 0)
    where
        dir = normalizeVec sdir
        dxinit = f' xinit dir
        pos :: Double -> Vec
        pos alpha = xinit ⊕ (alpha ⊙ dir)
        doublingSearch = [(a, f' (pos a) dir) | a <- iterate (*2) rfInitSigma]
        (a1,a2) = head (filter (\((_,dx),(_,dy)) -> (dx <= 0) && (dy >= 0)) (zip ((0, dxinit):doublingSearch) doublingSearch))
        secant (x,dx) (y,dy) = (x*dy - y*dx) / (dy - dx)
        rfs :: (Double, Double) -> (Double, Double) -> Int -> Double
        rfs (x,dx) (y,dy) bal
            | (dx == 0) = x
            | (dy == 0) = y
            | ((y-x) < epsilon) = secant (x,dx) (y,dy)
            | (dz <= 0) = rfs (z,dz) (y,dy) (min bal 0 - 1)
            | otherwise = rfs (x,dx) (z,dz) (max bal 0 + 1)
            where
                sy = if bal <= (-2) then 0.5 else 1
                sx = if bal >= 2 then 0.5 else 1
                z = (secant (x, sx*dx) (y, sy*dy))
                dz = f' (pos z) dir

-- nonlinear conjugate gradient search using Polak-Ribière method
-- fstar calculates function value and total derivative
-- f' calculates direcrtional derivatives
conjugateGradientSearch :: (Double, Double) -> (Vec -> (Double, Vec)) -> (Vec -> Vec -> Double) -> Vec -> Vec
conjugateGradientSearch (e1, e2) fstar f' start = cjs dims (start ⊕ Vec [2*e1]) zero zero start
    where
        dims = length (coords start)
        cjs :: Int -> Vec -> Vec -> Vec -> Vec -> Vec
        cjs bal oldx olddir oldgrad x = if (normVec (oldx ⊖ x) < e1 || normVec (x ⊖ newx) < e1) then newx else cjs nbal x sdir grad newx
            where
                (v,grad) = fstar x
                beta' = (innerProd grad (oldgrad ⊖ grad) / innerProd oldgrad oldgrad) --Polak-Ribière
                (beta, nbal) = if (bal >= dims || beta' <= 0) then (0,0) else (beta', bal + 1)
                sdir = (addinv grad) ⊕ (beta ⊙ olddir)
                newx = traceShow (x,v) (regulaFalsiSearch e2 f' x sdir)


llpLineSearch :: (Ix sigma) => M.Map Int (Int, [Int]) -> MaxentViolationCounter sigma -> Vec -> Vec -> Vec
llpLineSearch viols ctr weights sdir = regulaFalsiSearch 0.01 (lexLogProbPartialDeriv viols ctr) weights sdir

llpOptimizeWeights :: (Ix sigma) => M.Map Int (Int, [Int]) -> MaxentViolationCounter sigma -> Vec -> Vec
llpOptimizeWeights viols ctr initweights = conjugateGradientSearch (0.01, 0.005)
                                                                   (lexLogProbTotalDeriv viols ctr)
                                                                   (lexLogProbPartialDeriv viols ctr)
                                                                   initweights
