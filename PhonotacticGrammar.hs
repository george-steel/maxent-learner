{-# LANGUAGE ScopedTypeVariables, ExplicitForAll, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module PhonotacticGrammar where

import Ring
import Probability
import MaxentGrammar
import WeightedDFA
import Text.CSV
import Data.Array.IArray
import Data.Maybe
import Data.Tuple
import Data.List
import Data.Monoid
import Control.Monad
import Control.DeepSeq
import qualified Data.Map.Lazy as M


-- enumeration for feature states (can be +,-,0)
data FeatureState = FOff | FPlus | FMinus deriving (Enum, Eq, Ord, Read, Show)
instance NFData FeatureState where
    rnf fs = fs `seq` ()

-- using integer indexes to a lookup table to represent segments
newtype SegRef = Seg Int deriving (Eq, Ord, Read, Show, Ix, NFData)

data FeatureTable sigma = FeatureTable { featTable :: Array (SegRef,Int) FeatureState
                                       , featNames :: Array Int String
                                       , segNames :: Array SegRef sigma
                                       , featLookup :: M.Map String Int
                                       , segLookup :: M.Map sigma SegRef } deriving (Show)

-- segment references
srBounds :: FeatureTable sigma -> (SegRef, SegRef)
srBounds ft = bounds (segNames ft)

ftlook :: FeatureTable sigma -> SegRef -> Int -> FeatureState
ftlook ft sr fi = featTable ft ! (sr,fi)

-- convert a string of raw segments to a string of SrgRefs
segsToRefs :: (Ord sigma) => FeatureTable sigma -> [sigma] -> [SegRef]
segsToRefs ft = mapMaybe (\x -> M.lookup x (segLookup ft))

-- convert a string of SegRefs back to segments
refsToSegs :: FeatureTable sigma -> [SegRef] -> [sigma]
refsToSegs ft = fmap (segNames ft !) . filter (inRange (srBounds ft))





lstrip :: String -> String
lstrip (' ':xs) = lstrip xs
lstrip ('\t':xs) = lstrip xs
lstrip xs = xs

-- read feature table stored as csv
csvToFeatureTable :: (Ord sigma) => (String -> sigma) -> String -> Maybe (FeatureTable sigma)
csvToFeatureTable readSeg rawcsv = do
    Right parsedcsv <- return (parseCSV "" rawcsv)
    ((_:segcells) : rawfeatrecs) <- return (fmap (fmap lstrip) parsedcsv)
    let numsegs  = length segcells
    guard (numsegs > 0)
    let seglist  = listArray (Seg 1, Seg numsegs) (fmap readSeg segcells)
        featrecs = filter (\xs -> length xs == numsegs + 1) rawfeatrecs
        numfeats = length featrecs
    guard (numfeats > 0)
    let featlist = listArray (1, numfeats) (fmap head featrecs)
        ft       = array ((Seg 1,1), (Seg numsegs, numfeats)) $ do
                        (featidx, _:featdata) <- zip [1..] featrecs
                        (segidx, segfield) <- zip (fmap Seg [1..]) featdata
                        let fstate = case segfield of
                                        "+"   -> FPlus
                                        "✓"   -> FPlus
                                        "-"   -> FMinus
                                        _     -> FOff
                        return ((segidx, featidx), fstate)
    let segmap   = M.fromList (fmap swap (assocs seglist))
        featmap  = M.fromList (fmap swap (assocs featlist))
    guard (M.size segmap == rangeSize (bounds seglist))
    return (FeatureTable ft featlist seglist featmap segmap)



allFeatures :: FeatureTable sigma -> [String]
allFeatures ft = elems (featNames ft)


--------------------------------------------------------------------------------


data NaturalClass = NClass { isInverted :: Bool
                           , featureList :: [(FeatureState, String)]
                           } deriving (Eq, Ord)
instance NFData NaturalClass where
    rnf c@(NClass b fs) = b `seq` rnf fs

instance Show NaturalClass where
    show (NClass isNegated feats) = (if isNegated then "[¬ " else "[") ++ unwords (fmap showfeat feats) ++ "]"
        where showfeat (fs, fn) = (case fs of
                                        FPlus -> "+"
                                        FMinus -> "−"
                                        FOff -> "0")
                                  ++ fn

xor :: Bool -> Bool -> Bool
xor False p = p
xor True p = not p

classToSeglist :: FeatureTable sigma -> NaturalClass -> [SegRef]
classToSeglist ft (NClass isNegated cls) = force $ do
        c <- range (srBounds ft)
        guard (isNegated `xor` and [ftlook ft c fi == fs | (fs,fi) <- icls])
        return c
    where icls = do
            (s,fn) <- cls
            Just fi <- return (M.lookup fn (featLookup ft))
            return (s,fi)


type NGram = [NaturalClass]
data Glob = Glob { leftContexts :: [NGram]
                 , countedSequence :: NGram }
                 deriving (Eq)
instance NFData Glob where
    rnf (Glob ctxs cs) = rnf ctxs `seq` rnf cs

instance Show Glob where
    show (Glob ctxs cs) = intercalate "…" (fmap (>>= show) (ctxs ++ [cs]))

countGlobMatches :: FeatureTable sigma -> Glob -> SingleViolationCounter SegRef
countGlobMatches ft (Glob ctxs cs) = force $ buildDFA ictxs
    where
        ictxs = fmap (fmap (classToSeglist ft)) ctxs
        ics = fmap (classToSeglist ft) cs
        buildDFA gs = foldr gateLeftContext (countngrams (srBounds ft) ics) gs




ngrams  :: Int -> [a] -> [[a]]
ngrams  0  _       = [[]]
ngrams  _  []      = []
ngrams  n  (x:xs)  = fmap (x:) (ngrams (n-1) xs) ++ ngrams n xs

-- Enumerate all classes (and their inverses to a certain number of features
-- in inverse order of the humber of features the uninverted class contains.
-- Discards duplicates (having the same set of segments).
classesByGenerality :: FeatureTable sigma -> Int -> [(Int, NaturalClass)]
classesByGenerality ft maxfeats = force $ fmap (\((ns, _), c) -> (ns,c)) (M.assocs cls)
    where
        cls = M.fromListWith (flip const) $ do
            isInv <- [False,True]
            nf <- range (0, maxfeats)
            fs <- ngrams nf (elems (featNames ft))
            c <- fmap (NClass isInv) . forM fs $ \f -> [(FPlus,f), (FMinus,f)]
            let cs = classToSeglist ft c
            let ns = length cs
            guard (ns /= 0)
            return ((negate ns, cs), c)


--------------------------------------------------------------------------------

partitionLength :: Int -> Int -> [[Int]]
partitionLength n 0 = return [n]
partitionLength 0 _ = return []
partitionLength 1 _ = return [1]
partitionLength n b = do
    k <- range (1,n)
    ks <- partitionLength (n-k) (b-1)
    return (k:ks)

multiTake :: [Int] -> [a] -> [[a]]
multiTake [] _ = []
multiTake _ [] = []
multiTake (n:ns) xs = take n xs : multiTake ns (drop n xs)


-- UG functions to generate list of constraint candidates

localTrigramGlobs :: [(Int, NaturalClass)] -> [NaturalClass] -> [Glob]
localTrigramGlobs classes coreClasses = fmap snd . sortOn fst $ singles ++ doubles ++ tripples
    where
        singles = do
            (w,cls) <- classes
            let g = Glob [] [cls]
            guard (not (isInverted cls))
            return ((1,w),g)
        doubles = do
            (w1,cls1) <- classes
            (w2,cls2) <- classes
            guard (not (isInverted cls1 && isInverted cls2))
            return ((2,w1+w2), Glob [] [cls1,cls2])
        tripples = do
            (w1,cls1) <- classes
            (w2,cls2) <- classes
            (w,cls3) <- case () of
                 () | cls1 `elem` coreClasses -> do
                        (w3,cls3') <- classes
                        guard (not (isInverted cls2 && isInverted cls3'))
                        return (w2+w3, cls3')
                    | cls2 `elem` coreClasses -> do
                        (w3,cls3') <- classes
                        guard (not (isInverted cls1 && isInverted cls3'))
                        return (w1+w3, cls3')
                    | otherwise -> do
                        cls3' <- coreClasses
                        guard (not (isInverted cls1 && isInverted cls2))
                        return (w1+w2, cls3')
            return ((3,w), Glob [] [cls1,cls2,cls3])

localBigramGlobs :: [(Int, NaturalClass)] -> [NaturalClass] -> [Glob]
localBigramGlobs classes coreClasses = fmap snd . sortOn fst $ singles ++ doubles
    where
        singles = do
            (w,cls) <- classes
            let g = Glob [] [cls]
            guard (not (isInverted cls))
            return ((1,w),g)
        doubles = do
            (w1,cls1) <- classes
            (w2,cls2) <- classes
            guard (not (isInverted cls1 && isInverted cls2))
            return ((2,w1+w2), Glob [] [cls1,cls2])

nonlocalTrigramGlobs :: [(Int, NaturalClass)] -> [NaturalClass] -> [Glob]
nonlocalTrigramGlobs classes coreClasses = fmap snd . sortOn fst $ singles ++ doubles ++ brokendoubles ++ tripples
    where
        nonlocalpenalty = 1000
        singles = do
            (w,cls) <- classes
            let g = Glob [] [cls]
            guard (not (isInverted cls))
            return ((1,w),g)
        doubles = do
            (w1,cls1) <- classes
            (w2,cls2) <- classes
            guard (not (isInverted cls1 && isInverted cls2))
            return ((2,w1+w2), Glob [] [cls1,cls2])
        brokendoubles = do
            ((_,w), Glob [] [cls1,cls2]) <- doubles
            guard (not (isInverted cls1) && not (isInverted cls2))
            return ((2,w+nonlocalpenalty), Glob [[cls1]] [cls2])
        tripples = do
            (w1,cls1) <- classes
            (w2,cls2) <- classes
            (w,cls3) <- case () of
                 () | cls1 `elem` coreClasses -> do
                        (w3,cls3') <- classes
                        guard (not (isInverted cls2 && isInverted cls3'))
                        return (w2+w3, cls3')
                    | cls2 `elem` coreClasses -> do
                        (w3,cls3') <- classes
                        guard (not (isInverted cls1 && isInverted cls3'))
                        return (w1+w3, cls3')
                    | otherwise -> do
                        cls3' <- coreClasses
                        guard (not (isInverted cls1 && isInverted cls2))
                        return (w1+w2, cls3')
            return ((3,w), Glob [] [cls1,cls2,cls3])

nonlocalNGrams :: Int -> Int -> [(Int, NaturalClass)] -> [Glob]
nonlocalNGrams maxStars maxClasses candidateClasses = fmap snd . sortOn fst $ do
    nc <- range (1,maxClasses)
    fcls <- replicateM nc candidateClasses
    let cls = fmap snd fcls
        totalscore = sum (fmap fst fcls)
    ns <- partitionLength nc maxStars
    let ngs = multiTake ns cls
    return ((nc + length ngs, totalscore), Glob (init ngs) (last ngs))
