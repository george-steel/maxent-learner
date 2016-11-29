{-# LANGUAGE ScopedTypeVariables, ExplicitForAll, GeneralizedNewtypeDeriving, FlexibleInstances #-}

module WeightedDFA where

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Data.Ix
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed ()
import Data.List
import Data.Bits
import Data.Monoid
import Ring

-- initialize an array by caching a function
fnArray :: (Ix i, IArray a e) => (i,i) -> (i -> e) -> a i e
fnArray bds f = array bds (fmap (\x -> (x, f x)) (range bds))

-- turns a pair of interval tuples into an interval of pairs
xbd :: (a,a) -> (b,b) -> ((a,b), (a,b))
xbd (w,x) (y,z) = ((w,y), (x,z))




-- Type for deterministic finite state transducers.
-- The array maps from (state, character) -> (next state, weight output)
-- both state labels and characters to consume must be contiguous ranges.
data DFST q sigma k = DFST { initialState :: q
                           , transitionMatrix :: Array (q,sigma) (q,k)
                           , finalWeights :: Array q k
                           } deriving (Show)

instance (NFData q, NFData sigma, NFData k) => NFData (DFST q sigma k) where
    rnf (DFST q0 tm fw) = q0 `seq` rnf tm `seq` rnf fw


-- bounds for state labels
stateBounds :: (Ix q, Ix sigma) => DFST q sigma w -> (q,q)
stateBounds (DFST _ arr _) = let ((a,_), (b,_)) = bounds arr in (a,b)

-- boounds for accepted segments (characters)
segBounds :: (Ix q, Ix sigma) => DFST q sigma k -> (sigma,sigma)
segBounds (DFST _ arr _) = let ((_,a), (_,b)) = bounds arr in (a,b)

-- advance by one state and get weight output
transition :: (Ix q, Ix sigma) => DFST q sigma k -> q -> sigma -> (q,k)
transition (DFST _ arr _) s c = arr!(s,c)

advanceState :: (Ix q, Ix sigma) => DFST q sigma k -> q -> sigma -> q
advanceState (DFST _ arr _) s c = fst (arr!(s,c))


instance (Ix q, Ix sigma) => Functor (DFST q sigma) where
    fmap f (DFST q0 tm fw) = DFST q0 (fmap (fmap f) tm) (fmap f fw)


-- remove unreachable states and renumber as integers (starting from 1) using a mark-sweep algorithm
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



-- raw product construction
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

-- Product construction for two transducers, taking a combining function for the weights.
-- For boolean weights, use (&&) for intersection and (||) for union.
dfaProduct :: (Ix l1, Ix l2, Ix sigma) => (w1 -> w2 -> w3) -> DFST l1 sigma w1 -> DFST l2 sigma w2 -> DFST Int sigma w3
dfaProduct f dfa1 dfa2 = pruneUnreachable (rawIntersection f dfa1 dfa2)

nildfa :: (Ix sigma, Monoid k) => (sigma,sigma) -> DFST Int sigma k
nildfa (a,z) = DFST 1 tm fm
    where
        tm = fnArray ((1,a),(1,z)) (const (1,mempty))
        fm = fnArray (1,1) (const mempty)

-- transduce a string of segments where and output the product of the weights (as a Monoid)
transduceM :: (Ix q, Ix sigma, Monoid k) => DFST q sigma k -> [sigma] -> k
transduceM (DFST q0 tm fw) cs = mconcat ws <> (fw ! fq)
    where (fq, ws) = mapAccumL (curry (tm!)) q0 cs

-- transduce a string of segments where and output the product of the weights (as a Ring)
transduceR :: (Ix q, Ix sigma, Semiring k) => DFST q sigma k -> [sigma] -> k
transduceR (DFST q0 tm fw) cs = productR ws ⊗ (fw ! fq)
    where (fq, ws) = mapAccumL (curry (tm!)) q0 cs

-- creates a transducer to count occurrences of an n-gram.
-- Takes a sequence of classes each reperesented as a list of segments
countngrams :: forall sigma . (Ix sigma) => (sigma, sigma) -> [[sigma]] -> DFST Int sigma (Sum Int)
countngrams sbound classes = pruneUnreachable (DFST 0 tm fw)
    where
        n = length classes
        cls :: Array Int [sigma]
        cls = listArray (1,n) classes
        states = range (0, 2^(n-1) - 1)
        tm :: Array (Int, sigma) (Int, Sum Int)
        tm = array ((0, fst sbound), (2^(n-1) - 1, snd sbound)) $ do
            s <- states
            c <- range sbound
            let ns = sum $ do
                    b <- range (1, n-1)
                    guard (b == 1 || testBit s (b-2))
                    guard (c `elem` (cls!b))
                    return (2^(b-1))
                isfinal = (n == 1 || testBit s (n-2)) && (c `elem` (cls!n))
                w = Sum (if isfinal then 1 else 0)
            return ((s,c),(ns,w))
        fw = fnArray (0, 2^(n-1) - 1) (const mempty)

{- NO LONGER IN USE, USE ListGlob INSTEAD
gateLeftContext :: forall sigma w . (Ix sigma, Monoid w) => [[sigma]] -> DFST Int sigma w -> DFST Int sigma w
gateLeftContext classes olddfa@(DFST oldarr) = pruneUnreachable (DFST mergedarr)
    where
        sbound = segBounds olddfa
        n = length classes
        (oldstart, oldend) = labelBounds olddfa
        offset = oldstart - 2^(n-1)
        cls :: Array Int [sigma]
        cls = listArray (1,n) classes
        states = range (0, 2^(n-1) - 1)
        mergedarr :: Array (Int, sigma) (Int, w)
        mergedarr = array ((offset, fst sbound), (oldend, snd sbound)) $ assocs oldarr ++ do
            s <- states
            c <- range sbound
            let ns = sum $ do
                    b <- range (1, n-1)
                    guard (b == 1 || testBit s (b-2))
                    guard (c `elem` (cls!b))
                    return (2^(b-1))
                isfinal = (n == 1 || testBit s (n-2)) && (c `elem` (cls!n))
                ns' = if isfinal then oldstart else (ns + offset)
            return ((offset + s,c),(ns',mempty))
-}


-- used for statistical calculations over an entire dfa with ring weights (e.g. probabilities)
-- given an array mapping states to weights (e.g, a maxent distribution),
-- gives a new distribution after transducing an additional character
stepweights :: (Ix q, Ix sigma, Semiring k) => DFST q sigma k -> Array q k -> Array q k
stepweights dfa@(DFST _ tm _) prev = accumArray (⊕) zero (sbound) (fmap pathweight (range (bounds tm)))
    where
        sbound = stateBounds dfa
        pathweight (s,c) = let (ns,w) = tm!(s,c) in (ns, (prev!s) ⊗ w)

-- gives an array from states to weights with 1 in the first position and 0 elsewhere
initialWeightArray :: (Ix l, Ix sigma, Semiring w) => DFST l sigma w -> Array l w
initialWeightArray dfa = fnArray (stateBounds dfa) (\x -> if x == initialState dfa then one else zero)

-- converts to an NFA with all the arrows reversed
reverseTM :: (Ix q, Ix sigma) => DFST q sigma k -> Array (q,sigma) [(q,k)]
reverseTM (DFST _ arr _) = accumArray (flip (:)) [] (bounds arr) (fmap (\((s,c),(s',w)) -> ((s',c),(s,w))) (assocs arr))



-- quantifiers for globs
data GlobReps = GSingle | GPlus | GStar deriving (Enum, Eq, Ord, Read, Show)
instance NFData GlobReps where
    rnf gr = gr `seq` ()

-- glob of segment lists, nore generalized version of ngrams allowing for repeated classes as well as single ones.
data ListGlob sigma = ListGlob Bool Bool [(GlobReps, [sigma])] deriving (Eq, Ord)

instance NFData sigma => NFData (ListGlob sigma) where
    rnf (ListGlob isinit isfin parts) = isinit `seq` isfin `seq` rnf parts

-- show in regex format
instance Show (ListGlob Char) where
    show (ListGlob isinit isfin parts) = (guard isinit >> "^") ++ (showGP =<< parts) ++ (guard isfin >> "$")
        where showGP (rep, cs) = "[" ++ cs ++ "]" ++ case rep of GSingle -> ""
                                                                 GPlus -> "+"
                                                                 GStar -> "*"

--
matchCounter :: forall sigma . (Ix sigma, Show sigma) => (sigma,sigma) -> ListGlob sigma -> DFST Int sigma (Sum Int)
matchCounter cbound glob@(ListGlob isinit isfin gparts) = pruneUnreachable (DFST (followEpsilons 0) tm fw) where
    ngp = length gparts
    nns = if isfin then ngp + 1 else ngp
    maxq = 2^nns - 1 -- this alo works as a bitmask
    tmbound = (0,maxq) `xbd` cbound
    gparr = listArray (0,ngp-1) gparts :: Array Int (GlobReps, [sigma])
    -- glob to nfa using bitmask lists
    followEpsilons b | b >= ngp = bit b
                     | fst (gparr ! b) == GStar = bit b .|. followEpsilons (b+1)
                     | otherwise = bit b
    ntf (b,c) = (if (b == 0) && not isinit then bit 0 .|. followEpsilons 0 else 0)
                .|. if not (c `elem` snd (gparr ! b)) then 0 else case fst (gparr ! b) of
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
        w = if not isfin && testBit ns ngp then Sum 1 else Sum 0
    tm = fnArray tmbound dtf :: Array (Int,sigma) (Int, Sum Int)
    fwf s = if testBit s ngp then Sum 1 else Sum 0
    fw = fnArray (0,maxq) fwf
