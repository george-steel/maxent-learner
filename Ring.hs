{-# LANGUAGE FlexibleInstances, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Ring ( Additive(..)
            , AdditiveGroup(..)
            , Semiring(..)
            , Ring
            , RingModule(..)
            , RSum, RProd
            , sumR, productR
            , Vec(..), coords, fromInts, vec
            , innerProd, normVec, normalizeVec, consVec
            , showFVec
            ) where

--import Data.Foldable
--import Data.Monoid
import Numeric
import qualified Data.Vector.Unboxed as V

--------------------------------------------------------------------------------
-- hierarchy of typeclasses for abstract algebra

-- class for additive monoids
class Additive v where
    zero :: v
    (⊕) :: v -> v -> v

-- class for additive groups, useful for vectors and rings
class (Additive v) => AdditiveGroup v where
    addinv :: v -> v
    (⊖) :: v -> v -> v
    x ⊖ y = x ⊕ addinv y

-- typeclass for semirings. Num cannot be used as a general ring class since Num requires a norm and signum
class (Additive r) => Semiring r where-- adition
    one :: r
    (⊗) :: r -> r -> r --multiplication

-- sombines subtraction and multiplication
class (Semiring r, AdditiveGroup r) => Ring r

-- adds scalar multiplication
class (Ring r, AdditiveGroup v) => RingModule r v where
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
instance {-# OVERLAPPABLE #-} (Num r) => Semiring r where
    one = 1
    (⊗) = (*)
instance {-# OVERLAPPABLE #-} (Num r) => AdditiveGroup r where
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
instance (AdditiveGroup r, AdditiveGroup s) => AdditiveGroup (r,s) where
    addinv (a,b) = (addinv a, addinv b)
    (a,b) ⊖ (c,d) = (a ⊖ c, b ⊖ d)
instance (Ring r, Ring s) => Ring (r,s)
instance (RingModule r v, RingModule r w) => RingModule r (v,w) where
    a ⊙ (b,c) = (a ⊙ b, a ⊙ c)


--------------------------------------------------------------------------------

-- take rings as sum or product monoids
newtype RSum r = RSum r deriving (Ord, Eq, Show, Additive, AdditiveGroup, Semiring, Ring)
newtype RProd r = RProd r deriving (Ord, Eq, Show, Additive, AdditiveGroup, Semiring, Ring)

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


-- polynomial-like vector space
-- behaves like union of R < R² < R³ < …
newtype Vec = Vec (V.Vector Double) deriving (Eq, Read, Show)

coords :: Vec -> [Double]
coords (Vec xs) = V.toList xs

fromInts :: [Int] -> Vec
fromInts xs = Vec . V.fromList . fmap fromIntegral $ xs

vec :: [Double] -> Vec
vec = Vec . V.fromList

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

instance RingModule Double Vec where
    a ⊙ (Vec xs) = Vec (V.map (a *) xs)
instance RingModule Int Vec where
    a ⊙ (Vec xs) = Vec (V.map (fromIntegral a *) xs)


innerProd :: Vec -> Vec -> Double
innerProd (Vec xs) (Vec ys) = V.sum (V.zipWith (*) xs ys)

showFVec :: Maybe Int -> Vec -> String
showFVec prec (Vec xs) = "[" ++ (unwords . fmap (\x -> showFFloat prec x []) . V.toList $ xs) ++ "]"

normVec :: Vec -> Double
normVec x = sqrt (innerProd x x)

normalizeVec :: Vec -> Vec
normalizeVec x = if n == 0 then x else (1/n) ⊙ x
    where n = normVec x

consVec :: Double -> Vec -> Vec
consVec x (Vec xs) = Vec (V.cons x xs)
