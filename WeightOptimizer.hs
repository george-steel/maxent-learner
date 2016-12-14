{-# LANGUAGE ScopedTypeVariables, ExplicitForAll, BangPatterns #-}
module WeightOptimizer where

import Ring
import MaxentGrammar
import Data.List
import qualified Data.Map as M
import Data.Ix
import Debug.Trace
import qualified Data.Vector.Unboxed as V
import System.IO
import System.IO.Unsafe
import Numeric
import WeightedDFA
import Data.Array.IArray
import Probability

-- using conjugate gradient method ad described by Shewchuk in
-- "An Introduction to the Conjugate Gradient Method Without the Agonizing Pain"


-- length of starting guess for line search
rfInitSigma :: Double
rfInitSigma = 0.05

traceInline :: String -> a -> a
traceInline s x = unsafePerformIO $ do
    hPutStr stderr s
    hFlush stderr
    return x

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

-- nonlinear conjugate gradient search using Polak-Ribière method
-- fstar calculates function value and total derivative
-- f' calculates direcrtional derivatives
conjugateGradientSearch :: (Double, Double) -> (Vec -> (Vec, Bool)) -> (Vec -> (Double, Vec)) -> (Vec -> Vec -> Double) -> Vec -> Vec
conjugateGradientSearch (e1, e2) conproj fstar f' start = cjs dims (start ⊕ vec [2*e1]) zero zero start
    where                                       -- fake last step triggers restart and aviods stopping condition
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
                newx = traceInline (if beta <= 0 then "+" else "-") $ regulaFalsiSearch e2 f' x sdir
                --newx = trace (showFVec (Just 3) grad) $ regulaFalsiSearch e2 f' x sdir
                (newx', iscorr) = conproj newx
                nbal' = if iscorr then dims else nbal


--------------------------------------------------------------------------------

traceVec :: Int -> Vec -> a -> a
traceVec prec (Vec xs) y = trace ("[" ++ (unwords . fmap (\x -> showFFloat (Just prec) x []) . V.toList $ xs) ++ "]") y

zeroNeg :: Vec -> (Vec, Bool)
zeroNeg (Vec v) = (Vec (V.map (\x -> if x < 0.01 then 0 else x) v), V.any (\x -> x /= 0 && x < -0.01) v)
    where hasnegs = V.any (< 0) v

-- line search specialized for lexLogProb
llpLineSearch :: (Ix sigma) => Array Length Int -> Vec -> MulticountDFST sigma -> Vec -> Vec -> Vec
llpLineSearch lengths oviols ctr weights sdir = regulaFalsiSearch 0.01 (lexLogProbPartialDeriv lengths oviols ctr) weights sdir

-- calculate weights to maximize probability of lexicon.
-- takes starting position of search which MUST have the correct number of entries (do not use `zero`)
llpOptimizeWeights :: (Ix sigma) => Array Length Int -> PackedText sigma -> MulticountDFST sigma -> Vec -> Vec
llpOptimizeWeights lengths pwfs dfa initweights = let oviols = fromMC (transducePackedMulti dfa pwfs)
                                         in conjugateGradientSearch (0.01, 0.005)
                                                                    zeroNeg
                                                                    (lexLogProbTotalDeriv lengths oviols dfa)
                                                                    (lexLogProbPartialDeriv lengths oviols dfa)
                                                                    initweights
