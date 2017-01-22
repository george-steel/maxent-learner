{-------------------------------------------------------------------------------

UNIVERSAL GRAMMAR FUNCTIONS

Put these together to generate lists of candidate constraints for the learner

-------------------------------------------------------------------------------}

module Linguistics.PhonotacticLearner.UniversalGrammar.Generators where

import Linguistics.PhonotacticLearner.UniversalGrammar
import Linguistics.PhonotacticLearner.WeightedDFA
import Data.List
import Data.Array.IArray
import qualified Data.Map as M
import Control.Monad
import Control.DeepSeq

-- Enumerate all classes (and their inverses to a certain number of features
-- in inverse order of the humber of features the uninverted class contains.
-- Discards duplicates (having the same set of segments).
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


ugSingleClasses :: [(Int, NaturalClass,SegSet SegRef)] -> [(ClassGlob, ListGlob SegRef)]
ugSingleClasses cls = fmap snd . sortOn fst $ do
    (w,c,l) <- cls
    guard (not (isInverted c))
    let g = ClassGlob False False [(GSingle,c)]
        lg = ListGlob False False [(GSingle,l)]
    return (w,(g,lg))

ugEdgeClasses :: [(Int, NaturalClass,SegSet SegRef)] -> [(ClassGlob, ListGlob SegRef)]
ugEdgeClasses cls = fmap snd . sortOn fst $ do
    (w,c,l) <- cls
    guard (not (isInverted c))
    (isinit,isfin) <- [(False,True),(True,False)]
    let g = ClassGlob isinit isfin [(GSingle,c)]
        lg = ListGlob isinit isfin [(GSingle,l)]
    return (w,(g,lg))

ugBigrams :: [(Int, NaturalClass,SegSet SegRef)] -> [(ClassGlob, ListGlob SegRef)]
ugBigrams cls = fmap snd . sortOn fst $ do
    (w1,c1,l1) <- cls
    (w2,c2,l2) <- cls
    guard (not (isInverted c1 && isInverted c2))
    let g = ClassGlob False False [(GSingle,c1),(GSingle,c2)]
        lg = ListGlob False False [(GSingle,l1),(GSingle,l2)]
    return (w1+w2,(g,lg))

ugEdgeBigrams :: [(Int, NaturalClass,SegSet SegRef)] -> [(ClassGlob, ListGlob SegRef)]
ugEdgeBigrams cls = fmap snd . sortOn fst $ do
    (w1,c1,l1) <- cls
    (w2,c2,l2) <- cls
    guard (not (isInverted c1 && isInverted c2))
    (isinit,isfin) <- [(False,True),(True,False)]
    let g = ClassGlob isinit isfin [(GSingle,c1),(GSingle,c2)]
        lg = ListGlob isinit isfin [(GSingle,l1),(GSingle,l2)]
    return (w1+w2,(g,lg))

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

ugLongDistance :: [(Int, NaturalClass,SegSet SegRef)] -> [(NaturalClass,SegSet SegRef)] -> [(ClassGlob, ListGlob SegRef)]
ugLongDistance cls rcls = fmap snd . sortOn fst $ do
    (w1,c1,l1) <- cls
    (c2,l2) <- rcls
    (w3,c3,l3) <- cls
    let w = w1+w3
        g = ClassGlob False False [(GSingle,c1),(GPlus,c2),(GSingle,c3)]
        lg = ListGlob False False [(GSingle,l1),(GPlus,l2),(GSingle,l3)]
    return (w, (g,lg))

ugMiddleHayesWilson :: [(Int, NaturalClass,SegSet SegRef)] -> [(NaturalClass,SegSet SegRef)] -> [(ClassGlob, ListGlob SegRef)]
ugMiddleHayesWilson cls rcls = join [ ugSingleClasses cls
                                    , ugBigrams cls
                                    , ugLimitedTrigrams cls rcls]

ugEdgeHayesWilson :: [(Int, NaturalClass,SegSet SegRef)] -> [(NaturalClass,SegSet SegRef)] -> [(ClassGlob, ListGlob SegRef)]
ugEdgeHayesWilson cls rcls = join [ ugSingleClasses cls
                                  , ugEdgeClasses cls
                                  , ugBigrams cls
                                  , ugEdgeBigrams cls
                                  , ugLimitedTrigrams cls rcls]
