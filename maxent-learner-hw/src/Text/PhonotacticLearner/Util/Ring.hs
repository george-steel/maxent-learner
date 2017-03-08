{-# LANGUAGE FlexibleInstances, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

{-|
Module: Text.PhonotacticLearner.Util.Ring
Description: Classes for rings and modules and data type for ℝ*.
Copyright: © 2016-2017 George Steel and Peter Jurgec
License: GPL-2+
Maintainer: george.steel@gmail.com

This module provides a hierarchy of typeclasses to describe rings and their substructures without all of the additional structure found in 'Num' (which does not apply to most rings). 'Num' instances are automatically 'Ring' instances if no explicit instance is given, making 'Ring' a drop-in replacenent for 'Num' to make more generic functions.

A typeclass for algebraic modules (aka. vector spaces if over a field) is also included as well as a data type for the vector space ℝ[x]=ℝ∪ℝ²∪ℝ³∪… (usable for ℝⁿ although dimensions are not checked).

In order to not clash with Num or Arrow, this module uses the unicode circled operators '⊕', '⊖', '⊗', and '⊙'. If you are using XIM (Linux) or WinCompose, add the following lines to your .XCompose file:

> <Multi_key> <r> <plus> : "⊕"
> <Multi_key> <r> <period> : "⊙"
> <Multi_key> <r> <asterisk> : "⊗"
> <Multi_key> <r> <minus> : "⊖"
-}
module Text.PhonotacticLearner.Util.Ring (
      -- * Rings
      Additive(..)
    , AdditiveGroup(..)
    , Semiring(..)
    , Ring
    , RSum(..), RProd(..)
    , sumR, productR
    -- * Modules and vectors
    , RingModule(..)
    , Vec(..), coords, fromInts, vec
    , innerProd, normVec, normalizeVec, consVec
    , l1Vec, dl1Vec
    , showFVec
    ) where

import Numeric
import qualified Data.Vector.Unboxed as V
import Control.DeepSeq
import Data.Monoid

-- | Monoids with additive syntax
class Additive g where
    zero :: g -- ^ Additive identity
    (⊕) :: g -> g -> g -- ^ Addition

-- | Additive groups with inverses and suntraction
class (Additive g) => AdditiveGroup g where
    addinv :: g -> g -- ^ Additive inverse, more general version of 'negate'
    (⊖) :: g -> g -> g -- ^ Subtraction
    x ⊖ y = x ⊕ addinv y
    {-# INLINE (⊖) #-}

-- | Semirings with addition and multiplication (but not subtraction).
class (Additive r) => Semiring r where
    one :: r -- ^ Multiplicative identity
    (⊗) :: r -> r -> r -- ^ Multiplication

-- | Rings have both subtraction and multiplication
class (Semiring r, AdditiveGroup r) => Ring r

-- | A module over a 'Ring' r is an 'AdditiveGroup' which can be multiplied by scalars in r.
class (Ring r, AdditiveGroup v) => RingModule r v where
    (⊙) :: r -> v -> v -- ^ Scalar multiplication

-- | Every Ring is a module over itself.
instance (Ring r) => RingModule r r where
    (⊙) = (⊗)
--------------------------------------------------------------------------------

-- trivial ring
instance Additive () where
    zero = ()
    () ⊕ () = ()
instance Semiring () where
    one = ()
    () ⊗ () = ()
instance AdditiveGroup () where
    addinv () = ()
    () ⊖ () = ()
instance Ring ()

-- Boolean semiring
instance Additive Bool where
    zero = False
    (⊕) = (||)
instance Semiring Bool where
    one = True
    (⊗) = (&&)

-- make a superclass of Num
instance {-# OVERLAPPABLE #-} (Num r) => Additive r where
    zero = 0
    (⊕) = (+)
    {-# INLINE (⊕) #-}
instance {-# OVERLAPPABLE #-} (Num r) => Semiring r where
    one = 1
    (⊗) = (*)
    {-# INLINE (⊗) #-}
instance {-# OVERLAPPABLE #-} (Num r) => AdditiveGroup r where
    addinv = negate
    {-# INLINE addinv #-}
    (⊖) = (-)
    {-# INLINE (⊖) #-}
instance {-# OVERLAPPABLE #-} (Num r) => Ring r



-- ring cartesian product
instance (Additive r, Additive s) => Additive (r,s) where
    zero = (zero,zero)
    (a,b) ⊕ (c,d) = (a ⊕ c, b ⊕ d)
instance (Semiring r, Semiring s) => Semiring (r,s) where
    one = (one,one)
    (a,b) ⊗ (c,d) = (a ⊗ c, b ⊗ d)
instance (AdditiveGroup r, AdditiveGroup s) => AdditiveGroup (r,s) where
    addinv (a,b) = (addinv a, addinv b)
    (a,b) ⊖ (c,d) = (a ⊖ c, b ⊖ d)
instance (Ring r, Ring s) => Ring (r,s)


instance (RingModule r v, RingModule r w) => RingModule r (v,w) where
    a ⊙ (b,c) = (a ⊙ b, a ⊙ c)


--------------------------------------------------------------------------------

-- | 'Monoid' wrapper using addition, same as 'Sum'
newtype RSum r = RSum r deriving (Ord, Eq, Show, Additive, AdditiveGroup, Semiring, Ring)

-- | 'Monoid' wrapper using multiplication, same as 'Product'
newtype RProd r = RProd r deriving (Ord, Eq, Show, Additive, AdditiveGroup, Semiring, Ring)

instance (Additive r) => Monoid (RSum r) where
    mempty = RSum zero
    (RSum a) `mappend` (RSum b) = RSum (a ⊕ b)

instance (Semiring r) => Monoid (RProd r) where
    mempty = RProd one
    (RProd a) `mappend` (RProd b) = RProd (a ⊗ b)

-- | Fold using addition
sumR :: (Foldable f, Additive r) => f r -> r
sumR xs = let (RSum x) = foldMap RSum xs in x

-- | Fold using multiplication
productR :: (Foldable f, Semiring r) => f r -> r
productR xs = let (RProd x) = foldMap RProd xs in x

--------------------------------------------------------------------------------

-- | Since ℝ is a module over ℤ, using scalar multiplication can save a lot of coersion noise.
instance RingModule Int Double where
    x ⊙ y = fromIntegral x * y
    {-# INLINE (⊙) #-}

-- | Implements variable-length vectors. Addition batween vectors of different lengths occurs by letting ℝ⊆ℝ²⊆ℝ³⊆… by embedding each length in the space op polynomials.
newtype Vec = Vec {unVec :: V.Vector Double} deriving (Eq, Read, Show, NFData)

-- | Returns a list of coordinates.
coords :: Vec -> [Double]
coords (Vec xs) = V.toList xs

-- | Convert from a list of coordinates.
vec :: [Double] -> Vec
vec = Vec . V.fromList

-- | Convert from a list of integers.
fromInts :: [Int] -> Vec
fromInts xs = Vec . V.fromList . fmap fromIntegral $ xs

instance Additive Vec where
    zero = Vec V.empty
    (Vec xs) ⊕ (Vec ys)
        | V.null xs = Vec ys
        | V.null ys = Vec xs
        | lx == ly = Vec (V.zipWith (+) xs ys)
        | lx < ly = Vec (V.zipWith (+) xs (V.take lx ys) V.++ V.drop lx ys)
        | ly < lx = Vec (V.zipWith (+) ys (V.take ly xs) V.++ V.drop ly xs)
        where lx = V.length xs
              ly = V.length ys

instance AdditiveGroup Vec where
    addinv (Vec xs) = Vec (V.map negate xs)
    {-# INLINE addinv #-}

instance RingModule Double Vec where
    a ⊙ (Vec xs) = Vec (V.map (a *) xs)
    {-# INLINE (⊙) #-}
instance RingModule Int Vec where
    a ⊙ (Vec xs) = Vec (V.map (fromIntegral a *) xs)
    {-# INLINE (⊙) #-}

-- | Standard inner product on ℝⁿ.
innerProd :: Vec -> Vec -> Double
innerProd (Vec xs) (Vec ys) = V.sum (V.zipWith (*) xs ys)

-- | Show a vector to a certain precision, equivalent of 'showFFloat'.
showFVec :: Maybe Int -> Vec -> String
showFVec prec (Vec xs) = "[" ++ (unwords . fmap (\x -> showFFloat prec x []) . V.toList $ xs) ++ "]"

-- | Calculate the euclidean norm of a vector.
normVec :: Vec -> Double
normVec x = sqrt (innerProd x x)

-- | Taxicab norm.
l1Vec :: Vec -> Double
l1Vec (Vec xs) = V.sum (V.map abs xs)

-- | Gradient of taxicab norm.
dl1Vec :: Vec -> Vec
dl1Vec (Vec xs) = Vec (V.map signum xs)

-- | Return a vector of unit length pointing in the same direction.
normalizeVec :: Vec -> Vec
normalizeVec x = if n == 0 then x else (1/n) ⊙ x
    where n = normVec x

-- | Add a number to the begining of the list of coordinates.
consVec :: Double -> Vec -> Vec
consVec x (Vec xs) = Vec (V.cons x xs)
