{-# LANGUAGE ScopedTypeVariables, ExplicitForAll #-}

module WeightedDFA where

import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed ()
import Data.Ix
import Data.Tuple
import Data.Bits
import Data.Monoid
import Ring



-- Type for deterministic finite state transducers.
-- The array maps from (state, character) -> (next state, weight output)
-- both state labels and characters to consume must be contiguous ranges.
newtype WDFA l sigma w = WDFA (Array (l,sigma) (l,w)) deriving Show

-- bounds for state labels
labelBounds :: (Ix l, Ix sigma) => WDFA l sigma w -> (l,l)
labelBounds (WDFA arr) = let ((a,_), (b,_)) = bounds arr in (a,b)

-- boounds for accepted segments (characters)
segBounds :: (Ix l, Ix sigma) => WDFA l sigma w -> (sigma,sigma)
segBounds (WDFA arr) = let ((_,a), (_,b)) = bounds arr in (a,b)

-- advance by one state and get weight output
transition :: (Ix l, Ix sigma) => WDFA l sigma w -> l -> sigma -> (l,w)
transition (WDFA arr) s c = arr!(s,c)

advanceState :: (Ix l, Ix sigma) => WDFA l sigma w -> l -> sigma -> l
advanceState (WDFA arr) s c = fst (arr!(s,c))


-- apply function to the output weights
mapweights :: (Ix l, Ix sigma) => (w1 -> w2) -> WDFA l sigma w1 -> WDFA l sigma w2
mapweights f (WDFA arr) = WDFA (fmap (fmap f) arr)


-- remove unreachable states and renumber as integers (starting from 1) using a mark-sweep algorithm
pruneUnreachable :: forall l sigma w . (Ix l, Ix sigma) => WDFA l sigma w -> WDFA Int sigma w
pruneUnreachable dfa = WDFA (array arrbound (fmap newdfa (range arrbound)))
    where
        lbound = labelBounds dfa
        cbound = segBounds dfa
        reachable = runSTUArray $ do
            reached :: STUArray s l Bool <- newArray lbound False
            let dfs :: l -> ST s ()
                dfs n = do
                    writeArray reached n True
                    forM_ (range cbound) $ \c -> do
                        let n' = advanceState dfa n c
                        seen <- readArray reached n'
                        when (not seen) (dfs n')
                    return ()
            dfs (fst lbound)
            return reached
        keepstates :: [l] = filter (reachable!) (range lbound)
        nbound = (1,length keepstates)
        oldlabels :: Array Int l = listArray nbound keepstates
        newlabels :: Array l Int = array lbound (zip keepstates (range nbound))
        arrbound = timesbound nbound cbound
        newdfa (s,c) = let (t,w) = transition dfa (oldlabels!s) c in ((s,c), (newlabels!t, w))


timesbound :: (a,a) -> (b,b) -> ((a,b), (a,b))
timesbound (w,x) (y,z) = ((w,y), (x,z))

-- raw product construction
rawIntersection :: (Ix l1, Ix l2, Ix sigma) => (w1 -> w2 -> w3) -> WDFA l1 sigma w1 -> WDFA l2 sigma w2 -> WDFA (l1,l2) sigma w3
rawIntersection f dfa1 dfa2 = if cbound == cbound2 then WDFA (array arrbound (fmap newdfa (range arrbound)))
                                                   else error "Segment ranges must match"
    where
        lbound1 = labelBounds dfa1
        lbound2 = labelBounds dfa2
        cbound = segBounds dfa1
        cbound2 = segBounds dfa2
        nbound = timesbound lbound1 lbound2
        arrbound = timesbound nbound cbound
        newdfa ((s1,s2),c) = let (t1,w1) = transition dfa1 s1 c
                                 (t2,w2) = transition dfa2 s2 c
                             in (((s1,s2),c), ((t1,t2), f w1 w2))

-- Product construction for two transducers, taking a combining function for the weights.
-- For boolean weights, use (&&) for intersection and (||) for union.
dfaProduct :: (Ix l1, Ix l2, Ix sigma) => (w1 -> w2 -> w3) -> WDFA l1 sigma w1 -> WDFA l2 sigma w2 -> WDFA Int sigma w3
dfaProduct f dfa1 dfa2 = pruneUnreachable (rawIntersection f dfa1 dfa2)



-- transduce a string of segments where and output the product of the weights (as a Monoid)
transduce :: (Ix l, Ix sigma, Monoid w) => WDFA l sigma w -> [sigma] -> w
transduce dfa@(WDFA arr) cs = mconcat $ evalState (mapM trans cs) (fst (labelBounds dfa))
    where
        trans = state . tf
        tf c s = swap (arr!(s,c))

-- transduce a string of segments where and output the product of the weights (as a Ring)
transduceR :: (Ix l, Ix sigma, Semiring w) => WDFA l sigma w -> [sigma] -> w
transduceR dfa@(WDFA arr) cs = foldl (⊗) one $ evalState (mapM trans cs) (fst (labelBounds dfa))
    where
        trans = state . tf
        tf c s = swap (arr!(s,c))

-- creates a transducer to count occurrences of an n-gram.
-- Takes a sequence of classes each reperesented as a list of segments
countngrams :: forall sigma . (Ix sigma) => (sigma, sigma) -> [[sigma]] -> WDFA Int sigma (Sum Int)
countngrams sbound classes = pruneUnreachable (WDFA arr)
    where
        n = length classes
        cls :: Array Int [sigma]
        cls = listArray (1,n) classes
        states = range (0, 2^(n-1) - 1)
        --unBinary [] = 0
        --unBinary (True:x) = 1 + 2 * (unBinary x)
        --unBinary (False:x) = 2 * (unBinary x)
        arr :: Array (Int, sigma) (Int, Sum Int)
        arr = array ((0, fst sbound), (2^(n-1) - 1, snd sbound)) $ do
            s <- states
            c <- range sbound
            let ns = sum $ do
                    b <- range (1, n-1)
                    guard (b == 1 || testBit s (b-2))
                    guard (c `elem` (cls!(b)))
                    return (2^(b-1))
                isfinal = (testBit s (n-2)) && (c `elem` (cls!(n)))
                w = Sum (if isfinal then 1 else 0)
            return ((s,c),(ns,w))



-- used for statistical calculations over an entire dfa with ring weights (e.g. probabilities)
-- given an array mapping states to weights (e.g, a maxent distribution),
-- gives a new distribution after transducing an additional character
stepweights :: (Ix l, Ix sigma, Semiring w) => WDFA l sigma w -> Array l w -> Array l w
stepweights dfa@(WDFA arr) prev = accumArray (⊕) zero (sbound) (fmap pathweight (range (bounds arr)))
    where
        sbound = labelBounds dfa
        pathweight (s,c) = let (ns,w) = arr!(s,c) in (ns, (prev!s) ⊗ w)

-- gives an array from states to weights with 1 in the first position and 0 elsewhere
initialWeightArray :: (Ix l, Ix sigma, Semiring w) => WDFA l sigma w -> Array l w
initialWeightArray dfa = array sbound ((fst sbound, one) : fmap (\s -> (s,zero)) (tail (range sbound)))
    where sbound = labelBounds dfa

-- converts to an NFA with all the arrows reversed
reverseDFA :: (Ix l, Ix sigma) => WDFA l sigma w -> Array (l,sigma) [(l,w)]
reverseDFA (WDFA arr) = accumArray (flip (:)) [] (bounds arr) (fmap (\((s,c),(s',w)) -> ((s',c),(s,w))) (assocs arr))
