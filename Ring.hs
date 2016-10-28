{-# LANGUAGE FlexibleInstances, UndecidableInstances, GeneralizedNewtypeDeriving #-}
module Ring where

import Data.Foldable
import Data.Monoid


class Semiring r where
    zero :: r
    (<+>) :: r -> r -> r
    one :: r
    (<.>) :: r -> r -> r
    

-- trivial ring
instance Semiring () where
    zero = ()
    () <+> () = ()
    one = ()
    () <.> () = ()

instance Semiring Bool where
    zero = False
    one = True
    (<+>) = (||)
    (<.>) = (&&)

-- num is ring with a few weird operations
instance {-# OVERLAPPABLE #-} (Num r) => Semiring r where
    zero = 0
    (<+>) = (+)
    one = 1
    (<.>) = (*)

instance (Semiring r, Semiring s) => Semiring (r,s) where
    zero = (zero,zero)
    one = (one,one)
    (a,b) <+> (c,d) = (a <+> c, b <+> d)
    (a,b) <.> (c,d) = (a <.> c, b <.> d)


newtype RSum r = RSum r deriving (Ord, Eq, Show, Semiring, Ring)
newtype RProd r = RProd r deriving (Ord, Eq, Show, Semiring, Ring)

instance (Semiring r) => Monoid (RSum r) where
    mempty = RSum zero
    (RSum a) `mappend` (RSum b) = RSum (a <+> b)

instance (Semiring r) => Monoid (RProd r) where
    mempty = RProd one
    (RProd a) `mappend` (RProd b) = RProd (a <.> b)

sumR :: (Foldable f, Semiring r) => f r -> r
sumR xs = let (RSum x) = foldMap RSum xs in x

productR :: (Foldable f, Semiring r) => f r -> r
productR xs = let (RProd x) = foldMap RProd xs in x

--------------------------------------------------------------------------------

class (Semiring r) => Ring r where
    addinv :: r -> r
    (<->) :: r -> r -> r
    x <-> y = x <+> addinv y

instance Ring () where
    addinv () = ()
    () <-> () = ()

instance {-# OVERLAPPABLE #-} (Num r) => Ring r where
    addinv = negate
    (<->) = (-)

instance (Ring r, Ring s) => Ring (r,s) where
    addinv (a,b) = (addinv a, addinv b)
    (a,b) <-> (c,d) = (a <-> c, b <-> d)
    
