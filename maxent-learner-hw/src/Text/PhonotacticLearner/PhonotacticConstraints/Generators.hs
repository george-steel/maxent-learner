{-|
Module: Text.PhonotacticLearner.PhonotacticConstraints.Generators
Description: Generation of candidate constraint sets.
Copyright: © 2016-2017 George Steel and Peter Jurgec
License: GPL-2+
Maintainer: george.steel@gmail.com

Functions for generating sets of candidate constraint sets.
For efficiency, classes are reperesented as @('NaturalClass', 'SegSet' 'SegRef')@ pairs and
constraints are output as @('ClassGlob', 'ListGlob' 'SegRef')@ pairs, avoiding the need for repeated conversions and copying of classes.

The 'classesByGenreraity' function enumerates the classes defined by a feature table in a sensible order, removing duplicate descriptions of the same class. The ug functions then take these classes and then combine them imto globs in various ways.

-}

module Text.PhonotacticLearner.PhonotacticConstraints.Generators (
    CandidateSettings(..), candidateGrammar,
    ngrams,
    classesByGenerality,
    ugSingleClasses, ugBigrams,
    ugEdgeClasses, ugEdgeBigrams,
    ugLimitedTrigrams, ugLongDistance,

    ugHayesWilson,
) where

import Text.PhonotacticLearner.PhonotacticConstraints
import Text.PhonotacticLearner.DFST
import Data.Bits
import Data.List
import Data.Array.IArray
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Set as S
import Control.Monad
import Control.DeepSeq
import Control.Parallel

data CandidateSettings = CandidateSettings {
    useEdges :: Bool,
    useTrigrams :: Maybe [T.Text],
    useBroken :: Maybe [T.Text]
} deriving (Eq, Show)

candidateGrammar :: FeatureTable sigma -> CandidateSettings -> (Int , Int, [(ClassGlob, ListGlob SegRef)])
candidateGrammar ft (CandidateSettings edges mtri mbroken) = ncls `seq` rnf candidates `seq` (ncls, ncand, candidates) where
    cls = classesByGenerality ft 3
    ncls = length cls
    cand1 = ugSingleClasses cls
    cande1 = if edges then ugEdgeClasses cls else []
    cand2 = ugBigrams cls
    cande2 = if edges then ugEdgeBigrams cls else []
    cand3 = case mtri of
        Nothing -> []
        Just tri -> ugLimitedTrigrams cls (coreClassesFromFeats ft tri)
    candb = case mbroken of
        Nothing -> []
        Just broken -> ugLongDistance cls (coreClassesFromFeats ft broken)
    candidates = cls `pseq` (rnf cande2 `par` rnf cand3 `par` rnf candb `par` join [cand1,cande1,cand2,cande2,cand3,candb])
    ncand = length candidates

-- | Given a number n and a sequence, returns all subsewuences of length n.
ngrams  :: Int -> [a] -> [[a]]
ngrams  0  _       = [[]]
ngrams  _  []      = []
ngrams  n  (x:xs)  = fmap (x:) (ngrams (n-1) xs) ++ ngrams n xs

segsetFromInteger :: (SegRef,SegRef) -> Integer -> SegSet SegRef
segsetFromInteger b set = fnArray b (\(Seg i) -> testBit set i)


-- | Enumerate all classes (and their inverses) to a certain number of features
-- in descending order of the number of segments the uninverted class contains.
-- Discards duplicates (having the same set of segments).
--
-- Each segment is returned as a tripple with the (negated for sorting) numbet of segments in the class, the class label, and the set of segments it contains.
classesByGenerality :: FeatureTable sigma -> Int -> [(Int, (NaturalClass, SegSet SegRef))]
classesByGenerality ft maxfeats = force . fmap prepout $ (M.assocs cls)
    where
        prepout ((n, set), cls)= (n, (cls, segsetFromInteger b set))
        b = (srBounds ft)
        sr = range b
        mask = foldl' (.|.) zeroBits [bit i | Seg i <- sr]
        fsets = do
            (fi,fn) <- assocs (featNames ft)
            fs <- [FPlus, FMinus]
            let fset = foldl' (.|.) 0 [bit i | s@(Seg i) <- sr, ftlook ft s fi == fs]
            return ((fs,fn),fset)
        cls = M.fromListWith (const id) . force $ do
            isInv <- [False,True]
            nf <- range (0, maxfeats)
            (fs,sets) <- fmap unzip (ngrams nf fsets)
            let cls = NClass isInv fs
                set = (if isInv then mask else 0) `xor` (foldl' (.&.) mask sets)
                ns = popCount set
            guard (set /= 0)
            return ((negate ns, set), cls)

coreClassesFromFeats :: FeatureTable sigma -> [T.Text] -> [(NaturalClass, SegSet SegRef)]
coreClassesFromFeats ft feats = nubBy (\x y -> snd x == snd y) $ do
    c <- fmap (NClass False) $ [] : (curry return <$> [FPlus,FMinus] <*> feats)
    let sl = classToSeglist ft c
    guard $ or (elems sl)
    return (c,sl)

-- | Given a set of classes, return a set of globs matching those classes.
ugSingleClasses :: [(Int, (NaturalClass, SegSet SegRef))] -> [(ClassGlob, ListGlob SegRef)]
ugSingleClasses cls = fmap snd . sortOn fst . force $ do
    (w,(c,l)) <- cls
    guard (not (isInverted c))
    let g = ClassGlob False False [(GSingle,c)]
        lg = ListGlob False False [(GSingle,l)]
    return (w,(g,lg))

-- Given a set of classes, return a set of globs matching those globs at word boundaries. At most one class may be inverted.
ugEdgeClasses :: [(Int, (NaturalClass, SegSet SegRef))] -> [(ClassGlob, ListGlob SegRef)]
ugEdgeClasses cls = fmap snd . sortOn fst . force $ do
    (w,(c,l)) <- cls
    guard (not (isInverted c))
    (isinit,isfin) <- [(False,True),(True,False)]
    let g = ClassGlob isinit isfin [(GSingle,c)]
        lg = ListGlob isinit isfin [(GSingle,l)]
    return (w,(g,lg))

-- | Given a set of classes, return a set pf globs matching class pairs, ordered by total weight. At most one class may be inverted.
ugBigrams :: [(Int, (NaturalClass, SegSet SegRef))] -> [(ClassGlob, ListGlob SegRef)]
ugBigrams cls = fmap snd . sortOn fst . force $ do
    (w1,(c1,l1)) <- cls
    (w2,(c2,l2)) <- cls
    guard (not (isInverted c1 && isInverted c2))
    let g = ClassGlob False False [(GSingle,c1),(GSingle,c2)]
        lg = ListGlob False False [(GSingle,l1),(GSingle,l2)]
    return (w1+w2,(g,lg))

-- | Given a set of classes, return a set pf globs matching class pairs at word boundaries, ordered by total weight. At most one class may be inverted.
ugEdgeBigrams :: [(Int, (NaturalClass, SegSet SegRef))] -> [(ClassGlob, ListGlob SegRef)]
ugEdgeBigrams cls = fmap snd . sortOn fst . force $ do
    (w1,(c1,l1)) <- cls
    (w2,(c2,l2)) <- cls
    guard (not (isInverted c1 && isInverted c2))
    (isinit,isfin) <- [(False,True),(True,False)]
    let g = ClassGlob isinit isfin [(GSingle,c1),(GSingle,c2)]
        lg = ListGlob isinit isfin [(GSingle,l1),(GSingle,l2)]
    return (w1+w2,(g,lg))

-- | Given a set of classes ansd a smaller subset, return a set of globs matching trigrams of classes from the set where at least one class is contained in the subset.  At most one class may be inverted.
ugLimitedTrigrams :: [(Int, (NaturalClass, SegSet SegRef))] -> [(NaturalClass, SegSet SegRef)] -> [(ClassGlob, ListGlob SegRef)]
ugLimitedTrigrams cls rcls = fmap snd . sortOn fst . force $ do
    let rcls' = S.fromList (fmap fst rcls)
    (w1,(c1,l1)) <- cls
    (w2,(c2,l2)) <- cls
    (w,(c3,l3)) <- case () of
         () | S.member c1 rcls' -> do
                (w3,(c3',l3')) <- cls
                guard (not (isInverted c2 && isInverted c3'))
                return (w2+w3, (c3',l3'))
            | S.member c2 rcls' -> do
                (w3,(c3',l3')) <- cls
                guard (not (isInverted c1 && isInverted c3'))
                return (w1+w3, (c3',l3'))
            | otherwise -> do
                guard (not (isInverted c1 && isInverted c2))
                (c3',l3') <- rcls
                return (w1+w2, (c3',l3'))
    let g = ClassGlob False False [(GSingle,c1),(GSingle,c2),(GSingle,c3)]
        lg = ListGlob False False [(GSingle,l1),(GSingle,l2),(GSingle,l3)]
    return (w, (g,lg))

-- | Given two sets of classes, return globs matching a pair oc slasses in the first set separated by any number of occurrences of a class in the second set.  At most one class may be inverted. At most one class may be inverted.
-- This can lead to fairly large grammar DFAs when multiple such constraints are merged.
ugLongDistance :: [(Int, (NaturalClass, SegSet SegRef))] -> [(NaturalClass, SegSet SegRef)] -> [(ClassGlob, ListGlob SegRef)]
ugLongDistance cls rcls = fmap snd . sortOn fst . force $ do
    (w1,(c1,l1)) <- cls
    (c2,l2) <- rcls
    (w3,(c3,l3)) <- cls
    let w = w1+w3
        g = ClassGlob False False [(GSingle,c1),(GPlus,c2),(GSingle,c3)]
        lg = ListGlob False False [(GSingle,l1),(GPlus,l2),(GSingle,l3)]
    return (w, (g,lg))

{-ugMiddleHayesWilson :: [(Int, NaturalClass,SegSet SegRef)] -> [(NaturalClass,SegSet SegRef)] -> [(ClassGlob, ListGlob SegRef)]
ugMiddleHayesWilson cls rcls = join [ ugSingleClasses cls
                                    , ugBigrams cls
                                    , ugLimitedTrigrams cls rcls]
-}

-- | Combine the above functions (not including 'ugLongDistance') into the original candidate generator from the Hayes and Wilson paper.
ugHayesWilson :: [(Int, (NaturalClass, SegSet SegRef))] -> [(NaturalClass, SegSet SegRef)] -> [(ClassGlob, ListGlob SegRef)]
ugHayesWilson cls rcls = join [ ugSingleClasses cls
                                  , ugEdgeClasses cls
                                  , ugBigrams cls
                                  , ugEdgeBigrams cls
                                  , ugLimitedTrigrams cls rcls]
