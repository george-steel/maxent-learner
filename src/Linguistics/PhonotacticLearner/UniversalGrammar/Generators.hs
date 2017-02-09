{-|
Module: Linguistics.PhonotacticLearner.UniversalGrammar.Generators
Description: Generation of candidate constraint sets.
Copyright: © 2016-2017 George Steel and Peter Jurgec
License: GPL-2+
Maintainer: george.steel@gmail.com


-}

module Linguistics.PhonotacticLearner.UniversalGrammar.Generators (
    ngrams,
    classesByGenerality,
    ugSingleClasses, ugBigrams,
    ugEdgeClasses, ugEdgeBigrams,
    ugLimitedTrigrams, ugLongDistance,

    ugHayesWilson,
) where

import Linguistics.PhonotacticLearner.UniversalGrammar
import Linguistics.PhonotacticLearner.WeightedDFA
import Data.List
import Data.Array.IArray
import qualified Data.Map as M
import Control.Monad
import Control.DeepSeq

-- | Given a number n and a sequence, returns all subsewuences of length n.
ngrams  :: Int -> [a] -> [[a]]
ngrams  0  _       = [[]]
ngrams  _  []      = []
ngrams  n  (x:xs)  = fmap (x:) (ngrams (n-1) xs) ++ ngrams n xs

-- | Enumerate all classes (and their inverses) to a certain number of features
-- in descending order of the number of segments the uninverted class contains.
-- Discards duplicates (having the same set of segments).
--
-- Each segment is returned as a tripple with the (negated for sorting) numbet of segments in the class, the class label, and the set of segments it contains.
classesByGenerality :: FeatureTable sigma -> Int -> [(Int, NaturalClass, SegSet SegRef)]
classesByGenerality ft maxfeats = force $ fmap (\((ns, cs), c) -> (ns,c,cs)) (M.assocs cls)
    where
        cls = M.fromListWith (const id) $ do
            isInv <- [False,True]
            nf <- range (0, maxfeats)
            fs <- ngrams nf (elems (featNames ft))
            c <- fmap (NClass isInv) . forM fs $ \f -> [(FPlus,f), (FMinus,f)]
            let cs = classToSeglist ft c
            let ns = length . filter id . elems $ cs
            guard (ns /= 0)
            return ((negate ns, cs), c)

-- | Given a set of classes, return a set of globs matching those classes.
ugSingleClasses :: [(Int, NaturalClass,SegSet SegRef)] -> [(ClassGlob, ListGlob SegRef)]
ugSingleClasses cls = fmap snd . sortOn fst $ do
    (w,c,l) <- cls
    guard (not (isInverted c))
    let g = ClassGlob False False [(GSingle,c)]
        lg = ListGlob False False [(GSingle,l)]
    return (w,(g,lg))

-- Given a set of classes, return a set of globs matching those globs at word boundaries.
ugEdgeClasses :: [(Int, NaturalClass,SegSet SegRef)] -> [(ClassGlob, ListGlob SegRef)]
ugEdgeClasses cls = fmap snd . sortOn fst $ do
    (w,c,l) <- cls
    guard (not (isInverted c))
    (isinit,isfin) <- [(False,True),(True,False)]
    let g = ClassGlob isinit isfin [(GSingle,c)]
        lg = ListGlob isinit isfin [(GSingle,l)]
    return (w,(g,lg))

-- | Given a set of classes, return a set pf globs matching class pairs, ordered by total weight.
ugBigrams :: [(Int, NaturalClass,SegSet SegRef)] -> [(ClassGlob, ListGlob SegRef)]
ugBigrams cls = fmap snd . sortOn fst $ do
    (w1,c1,l1) <- cls
    (w2,c2,l2) <- cls
    guard (not (isInverted c1 && isInverted c2))
    let g = ClassGlob False False [(GSingle,c1),(GSingle,c2)]
        lg = ListGlob False False [(GSingle,l1),(GSingle,l2)]
    return (w1+w2,(g,lg))

-- | Given a set of classes, return a set pf globs matching class pairs at word boundaries, ordered by total weight.
ugEdgeBigrams :: [(Int, NaturalClass,SegSet SegRef)] -> [(ClassGlob, ListGlob SegRef)]
ugEdgeBigrams cls = fmap snd . sortOn fst $ do
    (w1,c1,l1) <- cls
    (w2,c2,l2) <- cls
    guard (not (isInverted c1 && isInverted c2))
    (isinit,isfin) <- [(False,True),(True,False)]
    let g = ClassGlob isinit isfin [(GSingle,c1),(GSingle,c2)]
        lg = ListGlob isinit isfin [(GSingle,l1),(GSingle,l2)]
    return (w1+w2,(g,lg))

-- | Given a set of classes ansd a smaller subset, return s set of globs matching trigrams of classes from the set where at least one class is contained in ste subset.
ugLimitedTrigrams :: [(Int, NaturalClass,SegSet SegRef)] -> [(NaturalClass,SegSet SegRef)] -> [(ClassGlob, ListGlob SegRef)]
ugLimitedTrigrams cls rcls = fmap snd . sortOn fst $ do
    (w1,c1,l1) <- cls
    (w2,c2,l2) <- cls
    (w,c3,l3) <- case () of
         () | (c1,l1) `elem` rcls -> do
                (w3,c3',l3') <- cls
                guard (not (isInverted c2 && isInverted c3'))
                return (w2+w3, c3',l3')
            | (c2,l2) `elem` rcls -> do
                (w3,c3',l3') <- cls
                guard (not (isInverted c1 && isInverted c3'))
                return (w1+w3, c3',l3')
            | otherwise -> do
                (c3',l3') <- rcls
                guard (not (isInverted c1 && isInverted c2))
                return (w1+w2, c3',l3')
    let g = ClassGlob False False [(GSingle,c1),(GSingle,c2),(GSingle,c3)]
        lg = ListGlob False False [(GSingle,l1),(GSingle,l2),(GSingle,l3)]
    return (w, (g,lg))

-- | Given two sets of classes, return globs matching a pair oc slasses in the first set separated by any number of occurrences of a class in the second set.
ugLongDistance :: [(Int, NaturalClass,SegSet SegRef)] -> [(NaturalClass,SegSet SegRef)] -> [(ClassGlob, ListGlob SegRef)]
ugLongDistance cls rcls = fmap snd . sortOn fst $ do
    (w1,c1,l1) <- cls
    (c2,l2) <- rcls
    (w3,c3,l3) <- cls
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
ugHayesWilson :: [(Int, NaturalClass,SegSet SegRef)] -> [(NaturalClass,SegSet SegRef)] -> [(ClassGlob, ListGlob SegRef)]
ugHayesWilson cls rcls = join [ ugSingleClasses cls
                                  , ugEdgeClasses cls
                                  , ugBigrams cls
                                  , ugEdgeBigrams cls
                                  , ugLimitedTrigrams cls rcls]
