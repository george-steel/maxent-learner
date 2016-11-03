{-# LANGUAGE ScopedTypeVariables, ExplicitForAll, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module PhonotacticGrammar where

import Ring
import MaxentGrammar
import Text.CSV
import Data.Array.IArray
import Data.Maybe
import Data.Tuple
import Control.Monad
import qualified Data.Map.Lazy as M



data FeatureState = FOff | FPlus | FMinus deriving (Enum, Eq, Ord, Read, Show)


newtype SegRef = Seg Int deriving (Eq, Ord, Read, Show, Ix)

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



newtype NaturalClass = NClass [(FeatureState, String)] deriving (Eq, Ord, Read, Show)

classToSeglist :: FeatureTable sigma -> NaturalClass -> [SegRef]
classToSeglist ft (NClass cls) = do
        c <- range (srBounds ft)
        guard (and [ftlook ft c fi == fs | (fs,fi) <- icls])
        return c
    where icls = do
            (s,fn) <- cls
            fi <- maybeToList (M.lookup fn (featLookup ft))
            return (s,fi)


type NGram = [(Bool, NaturalClass)]
data Glob = Glob { leftContexts :: [NGram]
                 , countedSequence :: NGram }
                 deriving (Eq, Read, Show)
