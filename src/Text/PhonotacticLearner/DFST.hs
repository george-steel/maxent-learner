{-# LANGUAGE ScopedTypeVariables, ExplicitForAll, FlexibleInstances, BangPatterns, FlexibleContexts, ForeignFunctionInterface, MultiParamTypeClasses, FunctionalDependencies #-}

{-|
Module: Text.PhonotacticLearner.DFST
Description: Library for handling deterministic finite state transducers
Copyright: © 2016-2017 George Steel and Peter Jurgec
License: GPL-2+
Maintainer: george.steel@gmail.com

Implementations of deterministic finite state transducers containing both a polymorphic 'DFST' functor and a typeclass for fast specialized types (with several implementations provided). Input alphabets are assumed to be finite rectangles inside 'Ix' types. Trandsuction functions are provided for both 'Monoid' and 'Semiring' output types. DFSTs may be created directly, generated from globs, or from smaller DFSTs using the product construction.

Optimized C functions for common output types (@'Sum' 'Int'@, 'Multicount', @'Expectation' 'Vec'@, and @'Expectation' 'Double'@) are included which use the 'PackedDFA' typeclass to convert to and from the generic type.  The specialized types additionally support the following operations.

    * Packing strings into a compact format ('PackedText')

    * Transducing packed strings into integer counts and Multicounts

    * Summing over all paths of expectation transducers

    * Applying maxent weights to vector counts to get expectations

-}

module Text.PhonotacticLearner.DFST (
    fnArray, xbd,

    -- * Polymorphic DFSTs
    DFST(..),
    stateBounds, segBounds, transition,
    transduceM, transduceR, --stepweights, initialWeightArray, reverseTM,

    -- * Specialized DFSTs
    PackedDFA(..), pruneUnreachable, pruneAndPack,
    rawIntersection, dfaProduct, nildfa,

    PackedText, packSingleText, packMultiText,
    ShortDFST, transducePackedShort,
    MulticountDFST, transducePackedMulti,
    ExpVecDFST, weightExpVec, expsByLengthVec,
    ExpDoubleDFST, weightExpPartial, expsByLengthDouble,

    -- * Glob recognition
    GlobReps(..), SegSet, ListGlob(..), matchCounter
)where

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Data.Ix
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as SVM
import qualified Data.Vector.Unboxed as V
import Foreign (Ptr)
import System.IO.Unsafe
import Data.List
import Data.Bits
import Data.Monoid
import Data.Int
import Control.Arrow ((&&&), first, second)

import Text.PhonotacticLearner.Util.Ring
import Text.PhonotacticLearner.Util.Probability

-- | Create an array by caching a function over a rectangle. Depending on the array type used, this can be used to memoise or precompute.
fnArray :: (Ix i, IArray a e) => (i,i) -> (i -> e) -> a i e
fnArray bds f = array bds (fmap (\x -> (x, f x)) (range bds))
{-# INLINE fnArray #-}

-- | Turn a pair of interval tuples into an interval of pairs. Used to compute array bounds for a cartesian product.
xbd :: (a,a) -> (b,b) -> ((a,b), (a,b))
xbd (w,x) (y,z) = ((w,y), (x,z))
{-# INLINE xbd #-}



-- | Polymorphic type for deterministic finite state transducers.
-- For an efficient implementation, the set of states and input characters are both limited to be 'Ix' rectangles and their product is the array bounds.
-- This type is a functor over its output type (note that transduction is only possible into a 'Monoid' or 'Semiring').
data DFST q sigma k = DFST { initialState :: q
                           , transitionMatrix :: Array (q,sigma) (q,k)
                           , finalWeights :: Array q k
                           } deriving (Show)

instance (NFData q, NFData sigma, NFData k) => NFData (DFST q sigma k) where
    rnf (DFST q0 tm fw) = q0 `seq` rnf tm `seq` rnf fw


instance (Ix q, Ix sigma) => Functor (DFST q sigma) where
    fmap f (DFST q0 tm fw) = DFST q0 (fmap (second f) tm) (fmap f fw)

-- | bounds for state labels
stateBounds :: (Ix q, Ix sigma) => DFST q sigma w -> (q,q)
stateBounds (DFST _ arr _) = let ((a,_), (b,_)) = bounds arr in (a,b)

-- | boounds for accepted segments (characters)
segBounds :: (Ix q, Ix sigma) => DFST q sigma k -> (sigma,sigma)
segBounds (DFST _ arr _) = let ((_,a), (_,b)) = bounds arr in (a,b)

-- | advance by one state and get weight output
transition :: (Ix q, Ix sigma) => DFST q sigma k -> q -> sigma -> (q,k)
transition (DFST _ arr _) s c = arr!(s,c)

advanceState :: (Ix q, Ix sigma) => DFST q sigma k -> q -> sigma -> q
advanceState (DFST _ arr _) s c = fst (arr!(s,c))

{-# INLINE stateBounds #-}
{-# INLINE segBounds #-}
{-# INLINE transition #-}
{-# INLINE advanceState #-}




-- | Structure holding text and word frequencies as a flat array of segment indices (in the input rectangle) for fast transduction.
data PackedText sigma = PackedText !(sigma,sigma) !(SV.Vector Int16) !(SV.Vector Int32)

-- | Pack a single string
packSingleText :: Ix sigma => (sigma,sigma) -> [sigma] -> PackedText sigma
packSingleText cbound t = PackedText cbound (SV.fromList $ fmap pchar t ++ [-1,-2]) (SV.singleton 1)
    where pchar = fromIntegral . index cbound

-- | Pack a list of string, fewquency pairs
packMultiText :: Ix sigma => (sigma,sigma) -> [([sigma],Int)] -> PackedText sigma
packMultiText cbound ts = PackedText cbound (SV.fromList $ foldr constext [-2] ts) (SV.fromList $ fmap (fromIntegral . snd) ts)
    where constext (t,_) lts  = fmap (fromIntegral . index cbound) t ++ [-1] ++ lts



-- | Typeclass for converting speaiclized DFSTs to and from polymorphic ones. This is used by several optimized versions for various output types that can be manupulated by fast C functions.
class PackedDFA pd k | pd -> k where
    -- | Number fo states in DFA
    numStates :: (Ix sigma) => pd sigma -> Int
    -- | Bounds of input rectangle
    psegBounds :: (Ix sigma) => pd sigma -> (sigma, sigma)
    -- | umpack
    unpackDFA :: (Ix sigma) => pd sigma -> DFST Int sigma k
    -- | Pack a DFA from its component functions. Use 'pruneAndPack' to convert a polymorphic 'DFST'.
    packDFA :: forall sigma . (Ix sigma)
            => Int16 -- number of states
            -> Int16 -- initial state
            -> (sigma, sigma) -- sigma bounds
            -> ((Int16,sigma) -> Int16) -- transition state
            -> ((Int16,sigma) -> k) -- transition weight
            -> (Int16 -> k) --final weight
            -> pd sigma





-- | Remove unreachable states and renumber as integers (starting from 1) using a mark-sweep algorithm. Result is polymorphic.
pruneUnreachable :: forall q sigma k . (Ix q, Ix sigma) => DFST q sigma k -> DFST Int sigma k
pruneUnreachable dfa = DFST (newlabels ! initialState dfa) newTM newFW
    where
        qbound = stateBounds dfa
        cbound = segBounds dfa
        reachable = runSTUArray $ do
            reached :: STUArray s q Bool <- newArray qbound False
            let dfs :: q -> ST s ()
                dfs n = do
                    writeArray reached n True
                    forM_ (range cbound) $ \c -> do
                        let n' = advanceState dfa n c
                        seen <- readArray reached n'
                        when (not seen) (dfs n')
                    return ()
            dfs (initialState dfa)
            return reached
        keepstates :: [q] = filter (reachable!) (range qbound)
        nbound = (1,length keepstates)
        oldlabels :: Array Int q = listArray nbound keepstates
        newlabels :: Array q Int = array qbound (zip keepstates (range nbound))
        tmbound = nbound `xbd` cbound
        newTF (s,c) = let (t,w) = transition dfa (oldlabels!s) c in (newlabels!t, w)
        newTM = fnArray tmbound newTF
        newFW = fnArray nbound ((finalWeights dfa !) . (oldlabels !))

-- | Prune unreachable states and pack into a specialized implementation
pruneAndPack :: forall q sigma pd k. (Ix q, Ix sigma, PackedDFA pd k) => DFST q sigma k -> pd sigma
pruneAndPack dfa = packDFA (fromIntegral ns) (newlabels ! initialState dfa) cbound newTF newTW newFW
    where
        qbound = stateBounds dfa
        cbound = segBounds dfa
        reachable = runSTUArray $ do
            reached :: STUArray s q Bool <- newArray qbound False
            let dfs :: q -> ST s ()
                dfs n = do
                    writeArray reached n True
                    forM_ (range cbound) $ \c -> do
                        let n' = advanceState dfa n c
                        seen <- readArray reached n'
                        when (not seen) (dfs n')
                    return ()
            dfs (initialState dfa)
            return reached
        keepstates :: [q] = filter (reachable!) (range qbound)
        ns = (length keepstates)
        nbound = (0, fromIntegral (ns - 1))
        oldlabels :: Array Int16 q = listArray nbound keepstates
        newlabels :: Array q Int16 = array qbound (zip keepstates (range nbound))
        newTF (s,c) = let (t,_) = transition dfa (oldlabels!s) c in newlabels!t
        newTW (s,c) = let (_,w) = transition dfa (oldlabels!s) c in w
        newFW = (finalWeights dfa !) . (oldlabels !) . fromIntegral


-- | Lifts a weight combining function into one that combines DFAs. Performs no pruning and new states are all pairs of old ones.
-- For boolean weights, use (&&) for intersection and (||) for union.
rawIntersection :: (Ix q1, Ix q2, Ix sigma) => (k1 -> k2 -> k3) -> DFST q1 sigma k1 -> DFST q2 sigma k2 -> DFST (q1,q2) sigma k3
rawIntersection f dfa1@(DFST qi1 tm1 fw1) dfa2@(DFST qi2 tm2 fw2)
        | cbound == cbound2 = DFST (qi1,qi2) (fnArray tmbound newTF) (fnArray qbound' newFW)
        | otherwise = error "Segment ranges must match"
    where
        qbound1 = stateBounds dfa1
        qbound2 = stateBounds dfa2
        cbound = segBounds dfa1
        cbound2 = segBounds dfa2
        qbound' = qbound1 `xbd` qbound2
        tmbound = qbound' `xbd` cbound
        newTF ((s1,s2),c) = let (t1,w1) = tm1 ! (s1,c)
                                (t2,w2) = tm2 ! (s2,c)
                             in ((t1,t2), f w1 w2)
        newFW (s1,s2) = f (fw1!s1) (fw2!s2)

-- | Product construction with pruning.
dfaProduct :: (Ix l1, Ix l2, Ix sigma) => (w1 -> w2 -> w3) -> DFST l1 sigma w1 -> DFST l2 sigma w2 -> DFST Int sigma w3
dfaProduct f dfa1 dfa2 = pruneUnreachable (rawIntersection f dfa1 dfa2)

-- | Given input bounds, Construct a DFST which always returns mempty for any string.
nildfa :: (Ix sigma, Monoid k) => (sigma,sigma) -> DFST Int sigma k
nildfa (a,z) = DFST 1 tm fm
    where
        tm = fnArray ((1,a),(1,z)) (const (1,mempty))
        fm = fnArray (1,1) (const mempty)

-- | Transduce a string of segments where and output the product of the weights (as a Monoid).
transduceM :: (Ix q, Ix sigma, Monoid k) => DFST q sigma k -> [sigma] -> k
transduceM (DFST q0 tm fw) cs = mconcat ws <> (fw ! fq)
    where (fq, ws) = mapAccumL (curry (tm!)) q0 cs

-- | Transduce a string of segments where and output the product of the weights (as a Ring).
transduceR :: (Ix q, Ix sigma, Semiring k) => DFST q sigma k -> [sigma] -> k
transduceR (DFST q0 tm fw) cs = productR ws ⊗ (fw ! fq)
    where (fq, ws) = mapAccumL (curry (tm!)) q0 cs



------------------------------------------------------------------------------
-- structurees for packed DFST counter with compact representation optimized c transduce implementation


-- | Optimized DFST specialized to transduce integers
data ShortDFST sigma = ShortDFST {-# UNPACK #-} !Int16 -- number of states
                                 {-# UNPACK #-} !Int16 -- initial state
                                 {-# UNPACK #-} !(sigma, sigma) -- segment bounds
                                 !(SV.Vector Int16) -- state transitions
                                 !(SV.Vector Int16) -- weight transitions
                                 !(SV.Vector Int16) -- final weights
                                 deriving (Show)


instance NFData sigma => NFData (ShortDFST sigma) where
    rnf (ShortDFST ns q0 sb tf tw fw) = ns `seq` q0 `seq` rnf sb `seq` tf `seq` tw `seq` fw `seq` ()

foreign import ccall unsafe "transducePackedShort"
    c_transducePackedShort :: Int16 -> Int16
        -> Ptr Int16 -> Ptr Int16 -> Ptr Int16
        -> Ptr Int16 -> Ptr Int32
        -> IO (Int64)

boxArray :: Ix i => (i,i) -> [(i,e)] -> Array i e
boxArray = array

instance PackedDFA ShortDFST (Sum Int) where
    numStates (ShortDFST ns _ _ _ _ _) = fromIntegral ns
    psegBounds (ShortDFST _ _ sb _ _ _) = sb

    packDFA ns' q0 cbound tf tw fw = ShortDFST ns' q0 cbound tfm twm fwm where
        ns = fromIntegral ns'
        nc = rangeSize cbound
        unIxc = boxArray (0,rangeSize cbound - 1) (fmap (index cbound &&& id) (range cbound))
        oldix i = let (qt,r) = i `quotRem` ns in (fromIntegral r, unIxc ! qt)
        tfm = SV.generate (ns*nc) (tf . oldix)
        twm = SV.generate (ns*nc) (fromIntegral . getSum . tw . oldix)
        fwm = SV.generate ns (fromIntegral . getSum . fw . fromIntegral)

    unpackDFA (ShortDFST ns q0 cbound tm twm fwm) = DFST (fromIntegral q0) tm' fwm' where
        qbound :: (Int, Int)
        qbound = (0, fromIntegral ns - 1)
        tbound = qbound `xbd` cbound
        --idx :: (Int,sigma) -> Int
        idx (s,c) = s + fromIntegral ns * index cbound c
        tf :: Int -> (Int, Sum Int)
        tf = (fromIntegral . (tm SV.!)) &&& (fromIntegral . (twm SV.!))
        tm' = fnArray tbound (tf . idx)
        fwm' = fnArray qbound (fromIntegral . (fwm SV.!))

-- | Fast transduction of integers. For multiple words, returns the sum of all transductions.
transducePackedShort :: (Ix sigma) => ShortDFST sigma -> PackedText sigma -> Int
transducePackedShort (ShortDFST ns q0 cb tf tw fw) (PackedText cb' tvec fvec)
    | cb == cb' = fromIntegral . unsafePerformIO $
        SV.unsafeWith tf $ \ptf -> SV.unsafeWith tw $ \ptw -> SV.unsafeWith fw $ \pfw ->
            SV.unsafeWith tvec $ \ptvec -> SV.unsafeWith fvec $ \pfvec ->
                c_transducePackedShort ns q0 ptf ptw pfw ptvec pfvec
    | otherwise = error "Mismatched chatacter bounds in packed text vs DFA"






-- | Optimized DFST specialized to transduce into 'Multicount' mnd count multiple quantities in parallel.
data MulticountDFST sigma = MulticountDFST {-# UNPACK #-} !Int16 -- number of states
                                           {-# UNPACK #-} !Int16 -- initial state
                                           {-# UNPACK #-} !(sigma, sigma) -- segment bounds
                                           {-# UNPACK #-} !Int16 -- vector dimensions
                                           !(SV.Vector Int16) -- state transitions
                                           !(SV.Vector Int16) -- weight transitions
                                           !(SV.Vector Int16) -- final weights
                                           deriving (Show)

instance NFData sigma => NFData (MulticountDFST sigma) where
    rnf (MulticountDFST ns q0 sb dims tf tw fw) = ns `seq` q0 `seq` rnf sb `seq` dims `seq` tf `seq` tw `seq` fw `seq` ()

instance PackedDFA MulticountDFST (Multicount) where
    numStates (MulticountDFST ns _ _ _ _ _ _) = fromIntegral ns
    psegBounds (MulticountDFST _ _ sb _ _ _ _) = sb

    packDFA ns' q0 cbound tf tw fw = MulticountDFST ns' q0 cbound (fromIntegral dims) tfm twm fwm where
        ns = fromIntegral ns'
        nc = rangeSize cbound
        dims::Int = let (MC x) = tw (0,fst cbound) in fromIntegral (V.length x)
        unIxc = boxArray (0,rangeSize cbound - 1) (fmap (index cbound &&& id) (range cbound))
        oldix i = let (qt,r) = i `quotRem` ns in (fromIntegral r, unIxc ! qt)
        tfm = SV.generate (ns*nc) (tf . oldix)
        twm = SV.generate (ns*nc*dims) (\i -> let (i',j) = i `quotRem` dims in fromIntegral (unMC (tw (oldix i')) V.! j))
        fwm = SV.generate (ns*dims) (\i -> let (i',j) = i `quotRem` dims in fromIntegral (unMC (fw (fromIntegral i')) V.! j))

    unpackDFA (MulticountDFST ns q0 cbound dims' tm twm fwm) = DFST (fromIntegral q0) tm' fwm' where
        dims = fromIntegral dims'
        qbound :: (Int, Int)
        qbound = (0, fromIntegral ns - 1)
        tbound = qbound `xbd` cbound
        --idx :: (Int,sigma) -> Int
        idx (s,c) = s + fromIntegral ns * index cbound c
        tf :: Int -> (Int, Multicount)
        tf i = (fromIntegral (tm SV.! i), MC ((V.map fromIntegral . SV.convert) (SV.slice (i*dims) dims twm)))
        tm' = fnArray tbound (tf . idx)
        fwm' = fnArray qbound (\i -> MC ((V.map fromIntegral . SV.convert) (SV.slice (i*dims) dims fwm)))

foreign import ccall unsafe "transducePackedMulti"
    c_transducePackedMulti :: Int16 -> Int16 -> Int16
        -> Ptr Int16 -> Ptr Int16 -> Ptr Int16
        -> Ptr Int16 -> Ptr Int32
        -> Ptr Int64 -> IO ()

-- | Fast transduction of 'Multicount'. For multiple words, returns the sum of all transductions.
transducePackedMulti :: (Ix sigma) => MulticountDFST sigma -> PackedText sigma -> Multicount
transducePackedMulti (MulticountDFST ns q0 cb dims tf tw fw) (PackedText cb' tvec fvec)
    | cb == cb' = MC . V.map fromIntegral . SV.convert . unsafePerformIO $
        SV.unsafeWith tf $ \ptf -> SV.unsafeWith tw $ \ptw -> SV.unsafeWith fw $ \pfw ->
            SV.unsafeWith tvec $ \ptvec -> SV.unsafeWith fvec $ \pfvec -> do
                outv :: SVM.IOVector Int64 <- SVM.new (fromIntegral dims)
                SVM.unsafeWith outv (c_transducePackedMulti ns q0 dims ptf ptw pfw ptvec pfvec)
                SV.freeze outv
    | otherwise = error "Mismatched chatacter bounds in packed text vs DFA"


-- | Optimized DFST form calculating verctor expectations over the entire probability distribution defined by the DFA.
data ExpVecDFST sigma = ExpVecDFST {-# UNPACK #-} !Int16 -- number of states
                                   {-# UNPACK #-} !Int16 -- initial state
                                   {-# UNPACK #-} !(sigma, sigma) -- segment bounds
                                   {-# UNPACK #-} !Int16 -- vector dimensions
                                   !(SV.Vector Int16) -- state transitions
                                   !(SV.Vector Double) -- transition probabilities
                                   !(SV.Vector Double) -- transition vectors
                                   !(SV.Vector Double) -- final probabilities
                                   !(SV.Vector Double) -- final vectors
                                   deriving (Show)

instance PackedDFA ExpVecDFST (Expectation Vec) where
    numStates (ExpVecDFST ns _ _ _ _ _ _ _ _ ) = fromIntegral ns
    psegBounds (ExpVecDFST _ _ sb _ _ _ _ _ _ ) = sb

    packDFA ns' q0 cbound tf tw fw = ExpVecDFST ns' q0 cbound (fromIntegral dims) tm tpm tvm fpm fvm where
        ns = fromIntegral ns'
        nc = rangeSize cbound
        dims::Int = let (Exp _ (Vec x)) = tw (0,fst cbound) in fromIntegral (V.length x)
        unIxc = boxArray (0,rangeSize cbound - 1) (fmap (index cbound &&& id) (range cbound))
        oldix i = let (qt,r) = i `quotRem` ns in (fromIntegral r, unIxc ! qt)
        tm = SV.generate (ns*nc) (tf . oldix)
        tpm = SV.generate (ns*nc) (prob . tw . oldix)
        tvm = SV.generate (ns*nc*dims) (\i -> let (i',j) = i `quotRem` dims in (unVec . exps . tw . oldix $ i') V.! j)
        fpm = SV.generate ns (prob . fw . fromIntegral)
        fvm = SV.generate (ns*dims) (\i -> let (i',j) = i `quotRem` dims in (unVec . exps . fw . fromIntegral $ i') V.! j)

    unpackDFA (ExpVecDFST ns q0 cbound dims' tm tpm tvm fpm fvm) = DFST (fromIntegral q0) tm' fwm' where
        dims = fromIntegral dims'
        qbound :: (Int, Int)
        qbound = (0, fromIntegral ns - 1)
        tbound = qbound `xbd` cbound
        --idx :: (Int,sigma) -> Int
        idx (s,c) = s + fromIntegral ns * index cbound c
        tf i = (fromIntegral (tm SV.! i), Exp (tpm SV.! i) (Vec (SV.convert (SV.slice (i*dims) dims tvm))))
        tm' = fnArray tbound (tf . idx)
        fw i = Exp (fpm SV.! i) (Vec (SV.convert (SV.slice (i*dims) dims fvm)))
        fwm' = fnArray qbound fw

--void weightExpVec(const int16_t ns, const int16_t nc, const int16_t dims,
--                  const int16_t* restrict tcounts, const int16_t* restrict fcounts,
--                  const double* restrict weights,
--                  double* restrict tprob, double* restrict tvec, double* restrict fprob, double* restrict fvec){

foreign import ccall unsafe "weightExpVec"
    c_weightExpVec :: Int16 -> Int16 -> Int16
        -> Ptr Int16 -> Ptr Int16
        -> Ptr Double
        -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> IO ()

-- | Assign maxent weights to the counts in a Multicount to get expectations (which include probabilities).
weightExpVec :: (Ix sigma) => MulticountDFST sigma -> Vec -> ExpVecDFST sigma
weightExpVec (MulticountDFST ns q0 cbound dims tm tc fc) (Vec weights)
    | dims' == V.length weights = (ExpVecDFST ns q0 cbound dims tm tpm tvm fpm fvm)
    where
        w' = SV.convert weights
        ns' = fromIntegral ns
        nc = rangeSize cbound
        dims' = fromIntegral dims
        (tpm,tvm,fpm,fvm) = unsafePerformIO $ do
            tpm' <- SVM.new (ns'*nc)
            tvm' <- SVM.new (ns'*nc*dims')
            fpm' <- SVM.new (ns')
            fvm' <- SVM.new (ns'*dims')
            SV.unsafeWith tc $ \ptc -> SV.unsafeWith fc $ \pfc -> SV.unsafeWith w' $ \pw ->
                SVM.unsafeWith tpm' $ \ptp -> SVM.unsafeWith tvm' $ \ptv -> SVM.unsafeWith fpm' $ \pfp -> SVM.unsafeWith fvm' $ \pfv ->
                    c_weightExpVec ns (fromIntegral nc) dims ptc pfc pw ptp ptv pfp pfv
            (,,,) <$> SV.freeze tpm' <*> SV.freeze tvm' <*> SV.freeze fpm' <*> SV.freeze fvm'

--void expsByLengthVec(const int16_t ns, const int16_t nc, const int16_t dims, const int16_t q0, const int16_t maxlen,
--                     const int16_t* restrict tmat, const double* restrict tprob, const double* restrict tvec, const double* restrict fprob, const double* restrict fvec,
--                     double* outp, double* outv) {
foreign import ccall unsafe "expsByLengthVec"
    c_expsByLengthVec :: Int16 -> Int16 -> Int16 -> Int16 -> Int16
        -> Ptr Int16 -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double
        -> Ptr Double -> Ptr Double -> IO ()

-- | Get the total expectations over each length of string up to a maximum
expsByLengthVec :: (Ix sigma) => ExpVecDFST sigma -> Int -> Array Int (Expectation Vec)
expsByLengthVec (ExpVecDFST ns q0 cbound dims tm tpm tvm fpm fvm) maxlen = unsafePerformIO $ do
    let nc = rangeSize cbound
        dims' = fromIntegral dims
        ns' = fromIntegral ns
    lpm <- SVM.new (ns'*(maxlen+1))
    lvm <- SVM.new (dims'*ns'*(maxlen+1))
    SV.unsafeWith tm $ \ptm -> SV.unsafeWith tpm $ \ptp -> SV.unsafeWith tvm $ \ptv -> SV.unsafeWith fpm $ \pfp -> SV.unsafeWith fvm $ \pfv ->
        SVM.unsafeWith lpm $ \plp -> SVM.unsafeWith lvm $ \plv ->
            c_expsByLengthVec ns (fromIntegral nc) dims q0 (fromIntegral maxlen) ptm ptp ptv pfp pfv plp plv
    fmap (listArray (0,maxlen)) . forM (range (0,maxlen)) $ \n -> do
        p <- SVM.read lpm n
        v <- SV.freeze (SVM.slice (n*dims') dims' lvm)
        return $ Exp p (Vec (SV.convert v))


-- | Optimized DFST form calculating scalar expectations over the entire probability distribution defined by the DFA.
data ExpDoubleDFST sigma = ExpDoubleDFST {-# UNPACK #-} !Int16 -- number of states
                                         {-# UNPACK #-} !Int16 -- initial state
                                         {-# UNPACK #-} !(sigma, sigma) -- segment bounds
                                         !(SV.Vector Int16) -- state transitions
                                         !(SV.Vector Double) -- transition probabilities
                                         !(SV.Vector Double) -- transition vectors
                                         !(SV.Vector Double) -- final probabilities
                                         !(SV.Vector Double) -- final vectors
                                         deriving (Show)

instance PackedDFA ExpDoubleDFST (Expectation Double) where
    numStates (ExpDoubleDFST ns _ _ _ _ _ _ _ ) = fromIntegral ns
    psegBounds (ExpDoubleDFST _ _ sb _ _ _ _ _ ) = sb

    packDFA ns' q0 cbound tf tw fw = ExpDoubleDFST ns' q0 cbound tm tpm tvm fpm fvm where
        ns = fromIntegral ns'
        nc = rangeSize cbound
        unIxc = boxArray (0,rangeSize cbound - 1) (fmap (index cbound &&& id) (range cbound))
        oldix i = let (qt,r) = i `quotRem` ns in (fromIntegral r, unIxc ! qt)
        tm = SV.generate (ns*nc) (tf . oldix)
        tpm = SV.generate (ns*nc) (prob . tw . oldix)
        tvm = SV.generate (ns*nc) (exps . tw . oldix)
        fpm = SV.generate ns (prob . fw . fromIntegral)
        fvm = SV.generate ns (exps . fw . fromIntegral)

    unpackDFA (ExpDoubleDFST ns q0 cbound tm tpm tvm fpm fvm) = DFST (fromIntegral q0) tm' fwm' where
        qbound :: (Int, Int)
        qbound = (0, fromIntegral ns - 1)
        tbound = qbound `xbd` cbound
        --idx :: (Int,sigma) -> Int
        idx (s,c) = s + fromIntegral ns * index cbound c
        tf i = (fromIntegral (tm SV.! i), Exp (tpm SV.! i) (tvm SV.! i))
        tm' = fnArray tbound (tf . idx)
        fw i = Exp (fpm SV.! i) (fvm SV.! i)
        fwm' = fnArray qbound fw

--void weightExpVec(const int16_t ns, const int16_t nc, const int16_t dims,
--                  const int16_t* restrict tcounts, const int16_t* restrict fcounts,
--                  const double* restrict weights,
--                  double* restrict tprob, double* restrict tvec, double* restrict fprob, double* restrict fvec){

foreign import ccall unsafe "weightExpPartial"
    c_weightExpPartial :: Int16 -> Int16 -> Int16
        -> Ptr Int16 -> Ptr Int16
        -> Ptr Double -> Ptr Double
        -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> IO ()

-- | Assign maxent weights to the counts in a Multicount to and apply a covector to the resulting expectations.
weightExpPartial :: (Ix sigma) => MulticountDFST sigma -> Vec -> Vec -> ExpDoubleDFST sigma
weightExpPartial (MulticountDFST ns q0 cbound dims tm tc fc) (Vec weights) (Vec dir)
    | dims' == V.length weights && dims' == V.length dir = (ExpDoubleDFST ns q0 cbound tm tpm tvm fpm fvm)
    where
        w' = SV.convert weights
        dir' = SV.convert dir
        ns' = fromIntegral ns
        nc = rangeSize cbound
        dims' = fromIntegral dims
        (tpm,tvm,fpm,fvm) = unsafePerformIO $ do
            tpm' <- SVM.new (ns'*nc)
            tvm' <- SVM.new (ns'*nc)
            fpm' <- SVM.new (ns')
            fvm' <- SVM.new (ns')
            SV.unsafeWith tc $ \ptc -> SV.unsafeWith fc $ \pfc -> SV.unsafeWith w' $ \pw -> SV.unsafeWith dir' $ \pdir ->
                SVM.unsafeWith tpm' $ \ptpm -> SVM.unsafeWith tvm' $ \ptvm -> SVM.unsafeWith fpm' $ \pfpm -> SVM.unsafeWith fvm' $ \pfvm ->
                    c_weightExpPartial ns (fromIntegral nc) dims ptc pfc pw pdir ptpm ptvm pfpm pfvm
            (,,,) <$> SV.freeze tpm' <*> SV.freeze tvm' <*> SV.freeze fpm' <*> SV.freeze fvm'




--vvoid expsByLengthDouble(const int16_t ns, const int16_t nc, const int16_t dims, const int16_t q0, const int16_t maxlen,
--                        const int16_t* restrict tmat, const double* restrict tprob, const double* restrict tvec, const double* restrict fprob, const double* restrict fvec,
--                        double* outp, double* outv) {
foreign import ccall unsafe "expsByLengthDouble"
    c_expsByLengthDouble :: Int16 -> Int16 -> Int16 -> Int16
        -> Ptr Int16 -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double
        -> Ptr Double -> Ptr Double -> IO ()

-- | Get the total expectations over each length of string up to a maximum
expsByLengthDouble :: (Ix sigma) => ExpDoubleDFST sigma -> Int -> Array Int (Expectation Double)
expsByLengthDouble (ExpDoubleDFST ns q0 cbound tm tpm tvm fpm fvm) maxlen = unsafePerformIO $ do
    let nc = rangeSize cbound
        ns' = fromIntegral ns
    lpm <- SVM.new (ns'*(maxlen+1))
    lvm <- SVM.new (ns'*(maxlen+1))
    SV.unsafeWith tm $ \ptm -> SV.unsafeWith tpm $ \ptp -> SV.unsafeWith tvm $ \ptv -> SV.unsafeWith fpm $ \pfp -> SV.unsafeWith fvm $ \pfv ->
        SVM.unsafeWith lpm $ \plp -> SVM.unsafeWith lvm $ \plv ->
            c_expsByLengthDouble ns (fromIntegral nc) q0 (fromIntegral maxlen) ptm ptp ptv pfp pfv plp plv
    fmap (listArray (0,maxlen)) . forM (range (0,maxlen)) $ \n -> do
        p <- SVM.read lpm n
        v <- SVM.read lvm n
        return (Exp p v)



--------------------------------------------------------------------------------


-- | Type for Glob quantifiers
data GlobReps = GSingle | GPlus | GStar deriving (Enum, Eq, Ord, Read, Show)
instance NFData GlobReps where
    rnf gr = gr `seq` ()

-- | Fast reperesentation of a set of segments by its characteristic function over an enclosing rectangle of segments.
type SegSet sigma = UArray sigma Bool

-- | Glob of segment lists, nore generalized version of ngrams allowing for repeated classes as well as single ones. The two boolean parameters restrict the glob to match a prefixes or suffixes only.
data ListGlob sigma = ListGlob Bool -- Is restricted to string start
                               Bool -- Is restricted to string end
                               [(GlobReps, SegSet sigma)] -- List of character sets and their quantifiers.
                               deriving (Eq, Ord)

instance (IArray UArray e, NFData i, Ix i) => NFData (UArray i e) where
    rnf a = let b = bounds a in (a ! fst b) `seq` rnf b

instance (NFData sigma, Ix sigma) => NFData (ListGlob sigma) where
    rnf (ListGlob isinit isfin parts) = isinit `seq` isfin `seq` rnf parts

-- | Globs are displayed in regex format
instance Show (ListGlob Char) where
    show (ListGlob isinit isfin parts) = (guard isinit >> "^") ++ (showGP =<< parts) ++ (guard isfin >> "$")
        where showGP :: (GlobReps, SegSet Char) -> String
              showGP (rep, cs) = "[" ++ fmap fst (filter snd (assocs cs)) ++ "]" ++
                                    case rep of GSingle -> ""
                                                GPlus -> "+"
                                                GStar -> "*"

-- | Create a DFST countign the violations of a ListGlob. Each 'SegSet' in the glob must have the same bounds and the glob must not be empty.
matchCounter :: forall sigma . (Ix sigma) => ListGlob sigma -> ShortDFST sigma
matchCounter (ListGlob isinit isfin gparts) = pruneAndPack $ DFST (followEpsilons 0) tm fw where
    cbound = bounds . snd . head $ gparts
    ngp = length gparts
    nns = if isfin then ngp + 1 else ngp
    maxq = 2^nns - 1 -- this alo works as a bitmask
    tmbound = (0,maxq) `xbd` cbound
    gparr = listArray (0,ngp-1) gparts :: Array Int (GlobReps, SegSet sigma)
    -- glob to nfa using bitmask lists
    followEpsilons b | b >= ngp = bit b
                     | fst (gparr ! b) == GStar = bit b .|. followEpsilons (b+1)
                     | otherwise = bit b
    ntf (b,c) = (if (b == 0) && not isinit then bit 0 .|. followEpsilons 0 else 0)
                .|. if not (snd (gparr ! b) ! c) then 0 else case fst (gparr ! b) of
                        GSingle -> followEpsilons (b+1)
                        GPlus -> followEpsilons b .|. followEpsilons (b+1)
                        GStar -> followEpsilons b
    ntm = fnArray ((0,ngp-1) `xbd` cbound) ntf :: Array (Int,sigma) Int
    --nfa to dfst
    dtf (s,c) = (ns',w) where
        ns = foldl (.|.) 0 $ do
            b <- range (0,ngp-1)
            guard (testBit s b)
            return (ntm ! (b,c))
        ns' = ns .&. maxq
        w = if not isfin && testBit ns ngp then 1 else 0
    tm = fnArray tmbound dtf :: Array (Int,sigma) (Int, Sum Int)
    fwf s = if testBit s ngp then 1 else 0
    fw = fnArray (0,maxq) fwf
