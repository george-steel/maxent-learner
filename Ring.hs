{-# LANGUAGE FlexibleInstances, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Ring ( Additive(..)
            , Subtractive(..)
            , Semiring(..)
            , Ring
            , RingModule(..)
            , RSum, RProd
            , sumR, productR
            , Vec(..), fromInts
            , innerProd, normVec, normalizeVec
            , addThese
            ) where

--import Data.Foldable
--import Data.Monoid
import Data.These
import Data.Align

--------------------------------------------------------------------------------
-- hierarchy of typeclasses for abstract algebra

-- class for additive monoids
class Additive v where
    zero :: v
    (⊕) :: v -> v -> v

-- class for additive groups, useful for vectors and rings
class (Additive v) => Subtractive v where
    addinv :: v -> v
    (⊖) :: v -> v -> v
    x ⊖ y = x ⊕ addinv y

-- typeclass for semirings. Num cannot be used as a general ring class since Num requires a norm and signum
class (Additive r) => Semiring r where-- adition
    one :: r
    (⊗) :: r -> r -> r --multiplication

-- sombines subtraction and multiplication
class (Semiring r, Subtractive r) => Ring r

-- adds scalar multiplication
class (Ring r, Subtractive v) => RingModule r v where
    (⊙) :: r -> v -> v

-- every Ring is a module over itself
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
instance Subtractive () where
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
instance {-# OVERLAPPABLE #-} (Num r) => Semiring r where
    one = 1
    (⊗) = (*)
instance {-# OVERLAPPABLE #-} (Num r) => Subtractive r where
    addinv = negate
    (⊖) = (-)
instance {-# OVERLAPPABLE #-} (Num r) => Ring r



-- ring cartesian product
instance (Additive r, Additive s) => Additive (r,s) where
    zero = (zero,zero)
    (a,b) ⊕ (c,d) = (a ⊕ c, b ⊕ d)
instance (Semiring r, Semiring s) => Semiring (r,s) where
    one = (one,one)
    (a,b) ⊗ (c,d) = (a ⊗ c, b ⊗ d)
instance (Subtractive r, Subtractive s) => Subtractive (r,s) where
    addinv (a,b) = (addinv a, addinv b)
    (a,b) ⊖ (c,d) = (a ⊖ c, b ⊖ d)
instance (Ring r, Ring s) => Ring (r,s)
instance (RingModule r v, RingModule r w) => RingModule r (v,w) where
    a ⊙ (b,c) = (a ⊙ b, a ⊙ c)


--------------------------------------------------------------------------------

-- take rings as sum or product monoids
newtype RSum r = RSum r deriving (Ord, Eq, Show, Additive, Subtractive, Semiring, Ring)
newtype RProd r = RProd r deriving (Ord, Eq, Show, Additive, Subtractive, Semiring, Ring)

instance (Additive r) => Monoid (RSum r) where
    mempty = RSum zero
    (RSum a) `mappend` (RSum b) = RSum (a ⊕ b)

instance (Semiring r) => Monoid (RProd r) where
    mempty = RProd one
    (RProd a) `mappend` (RProd b) = RProd (a ⊗ b)

-- fold over ring operations, more general versiond od sum and product from Num
sumR :: (Foldable f, Additive r) => f r -> r
sumR xs = let (RSum x) = foldMap RSum xs in x

productR :: (Foldable f, Semiring r) => f r -> r
productR xs = let (RProd x) = foldMap RProd xs in x

--------------------------------------------------------------------------------

-- Double is a module over Int, saves a lot of fromIntegral noise
instance RingModule Int Double where
    x ⊙ y = fromIntegral x * y


addThese :: (Num a) => These a a -> a
addThese (This a) = a
addThese (That b) = b
addThese (These a b) = a + b


-- polynomial-like vector space
-- behaves like union of R < R² < R³ < …
newtype Vec = Vec {coords ::[Double]} deriving (Eq, Read, Show)

fromInts :: [Int] -> Vec
fromInts xs = Vec (fmap fromIntegral xs)

instance Additive Vec where
    zero = Vec []
    (Vec xs) ⊕ (Vec ys) = Vec (alignWith addThese xs ys)
instance Subtractive Vec where
    addinv (Vec xs) = Vec (fmap negate xs)

instance RingModule Double Vec where
    a ⊙ (Vec xs) = Vec (fmap (a *) xs)
instance RingModule Int Vec where
    a ⊙ (Vec xs) = Vec (fmap (fromIntegral a *) xs)


innerProd :: Vec -> Vec -> Double
innerProd (Vec xs) (Vec ys) = sum (zipWith (*) xs ys)

normVec :: Vec -> Double
normVec x = sqrt (innerProd x x)

normalizeVec :: Vec -> Vec
normalizeVec x = if n == 0 then x else (1/n) ⊙ x
    where n = normVec x
