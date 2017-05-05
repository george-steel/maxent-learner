{-# LANGUAGE ScopedTypeVariables, ExplicitForAll, MultiParamTypeClasses, GeneralizedNewtypeDeriving, OverloadedStrings #-}

{-|
Module: Text.PhonotacticLearner.PhonotacticConstraints
Description: Description of phonological features and consrtraints.
Copyright: © 2016-2017 George Steel and Peter Jurgec
License: GPL-2+
Maintainer: george.steel@gmail.com

Data structures and functions for working with phonological features and natural classes.

Feature tables are designed work with strings reperesented as lists of 'SegRef' indices into their internal segment lists, enabling processing with fast array lookups even with non-contiguous sets of segments.
-}

module Text.PhonotacticLearner.PhonotacticConstraints (
    -- * Phonological Features
    FeatureState(..), SegRef(..),

    FeatureTable(..), srBounds, ftlook,
    segsToRefs, refsToSegs,
    csvToFeatureTable, featureTableToCsv,

    -- * Natural Classes
    NaturalClass(..), classToSeglist,
    GlobReps(..), ClassGlob(..), classesToLists,

    cgMatchCounter
) where

import Text.PhonotacticLearner.Util.Ring
import Text.PhonotacticLearner.Util.Probability
import Text.PhonotacticLearner.MaxentGrammar
import Text.PhonotacticLearner.DFST

import Text.Read.CSV
import Data.Array.IArray
import Data.Maybe
import Data.Tuple
import Data.List
import Data.Char
import Data.Monoid
import Control.Monad
import Control.Applicative hiding (many, some)
import Control.DeepSeq
import Control.Arrow((***),(&&&),first,second)
import qualified Data.Text as T
import qualified Data.Map.Lazy as M
import Text.ParserCombinators.ReadP
import Text.Read(Read(..),lift,parens,readMaybe)


-- | Enumeration for feature states (can be +,-,0)
data FeatureState = FOff | FPlus | FMinus deriving (Enum, Eq, Ord, Read, Show)
instance NFData FeatureState where
    rnf fs = fs `seq` ()

-- | Indices for segment lookup table
newtype SegRef = Seg Int deriving (Eq, Ord, Read, Show, Ix, NFData)

-- | Type for phonological feature table. Segments and features are referred to by indices so this structure includes lookup tables for those.
data FeatureTable sigma = FeatureTable { featTable :: Array (SegRef,Int) FeatureState
                                       , featNames :: Array Int T.Text
                                       , segNames :: Array SegRef sigma
                                       , featLookup :: M.Map T.Text Int
                                       , segLookup :: M.Map sigma SegRef } deriving (Show, Eq)

instance (Ord a, NFData a) => NFData (FeatureTable a) where
    rnf (FeatureTable ft fn sn fl sl) = rnf ft `seq` rnf fn `seq` rnf sn `seq` rnf fl `seq` rnf sl

-- | Bounds for segment references
srBounds :: FeatureTable sigma -> (SegRef, SegRef)
srBounds ft = bounds (segNames ft)
{-# INLINE srBounds #-}

-- | Shortcut for feature table array access
ftlook :: FeatureTable sigma -> SegRef -> Int -> FeatureState
ftlook ft sr fi = featTable ft ! (sr,fi)
{-# INLINE ftlook #-}

-- | Convert a string of raw segments to a string of 'SegRef's. Skips unrecognisable segments.
segsToRefs :: (Ord sigma) => FeatureTable sigma -> [sigma] -> [SegRef]
segsToRefs ft = mapMaybe (\x -> M.lookup x (segLookup ft))

-- | Convert a string of 'SegRef's back to segments
refsToSegs :: FeatureTable sigma -> [SegRef] -> [sigma]
refsToSegs ft = fmap (segNames ft !) . filter (inRange (srBounds ft))

-- | List of all features in table
allFeatures :: FeatureTable sigma -> [T.Text]
allFeatures ft = elems (featNames ft)




lstrip :: String -> String
lstrip (' ':xs) = lstrip xs
lstrip ('\t':xs) = lstrip xs
lstrip xs = xs

{- |
Parse feature table from CSV.

To use a feature table other than the default IPA one, you may define it in CSV format (RFC 4180). The segment names are defined by the first row (they may be any strings as long as they are all distinct, i.e. no duplicate names) and the feature names are defined by the first column (they are not hard-coded). Data cells should contain @+@, @-@, or @0@ for binary features and @+@ or @0@ for privative features (where we do not want a minus set that could form classes).

As a simple example, consider the following CSV file, defining three segments (a, n, and t), and two features (vowel and nasal).

>      ,a,n,t
> vowel,+,-,-
> nasal,0,+,-

If a row contains a different number of cells (separated by commas) than the header line, is rejected as invalid and does not define a feature (and will not be dispayed in the formatted feature table). If the CSV which is entered has duplicate segment names, no segments, or no valid features, the entire table is rejected (indicated by a red border around the text area, green is normal) and the last valid table is used and displayed.
-}
csvToFeatureTable :: (Ord sigma) => (String -> sigma) -> String -> Maybe (FeatureTable sigma)
csvToFeatureTable readSeg rawcsv = do
    parsedcsv <- readCSV rawcsv
    ((_:segcells) : rawfeatrecs) <- return (fmap (fmap lstrip) parsedcsv)
    let numsegs  = length segcells
    guard (numsegs > 0)
    let seglist  = listArray (Seg 1, Seg numsegs) (fmap readSeg segcells)
        featrecs = filter (\xs -> length xs == numsegs + 1) rawfeatrecs
        numfeats = length featrecs
    guard (numfeats > 0)
    let featlist = listArray (1, numfeats) (fmap (T.pack . head) featrecs)
        ft       = array ((Seg 1,1), (Seg numsegs, numfeats)) $ do
                        (featidx, _:featdata) <- zip [1..] featrecs
                        (segidx, segfield) <- zip (fmap Seg [1..]) featdata
                        let fstate = case segfield of
                                        "+"   -> FPlus
                                        "✓"   -> FPlus
                                        "√"   -> FPlus
                                        "-"   -> FMinus
                                        "−"   -> FMinus
                                        _     -> FOff
                        return ((segidx, featidx), fstate)
    let segmap   = M.fromList (fmap swap (assocs seglist))
        featmap  = M.fromList (fmap swap (assocs featlist))
    guard (M.size segmap == rangeSize (bounds seglist))
    return (FeatureTable ft featlist seglist featmap segmap)

fschar FPlus = "+"
fschar FMinus = "-"
fschar FOff = "0"

-- | Cave a modified feature table to CSV format
featureTableToCsv :: (sigma -> String) -> FeatureTable sigma -> String
featureTableToCsv writeSeg ft = writeCSV (header : body) where
    header = "" : fmap writeSeg (elems (segNames ft))
    body = [T.unpack fn : [fschar (ftlook ft s f) | s <- indices (segNames ft)] | (f,fn) <- assocs (featNames ft)]

-- | Parse a list of strings (one per line) into a list of SegRef strings and frequencies.
-- Takes a feature table and a function to divide strings into segments (for single character segments, @fmap return@ may be used).
-- Lines in the list may be optionally followed by a tab and an integer indicating their frequendy (instead of suplicating lines).play table

readSrLexicon :: FeatureTable String -> (String -> [String]) -> String -> [([SegRef],Int)]
readSrLexicon ft seg text = do
    line <- lines text
    let (wt@(_:_),wf') = break (== '\t') line
    n <- case (words wf') of
            [] -> [1]
            [wf] -> maybeToList $ readMaybe wf
            _ -> []
    return (segsToRefs ft (seg wt), n)


--------------------------------------------------------------------------------

-- | Representation of a natural class as a list of features and their states. Can ahso handle inverted classes.
data NaturalClass = NClass { isInverted :: {-# UNPACK #-} !Bool
                           , featureList :: [(FeatureState, T.Text)]
                           } deriving (Eq, Ord)

instance NFData NaturalClass where
    rnf (NClass b fs) = b `seq` rnf fs

-- | Uses SPE format
instance Show NaturalClass where
    show (NClass isNegated feats) = (if isNegated then "[¬ " else "[") ++ unwords (fmap showfeat feats) ++ "]"
        where showfeat (fs, fn) = (case fs of
                                        FPlus -> "+"
                                        FMinus -> "−"
                                        FOff -> "0")
                                  ++ T.unpack fn

isPrintNelem :: String -> Char -> Bool
isPrintNelem s c =  isPrint c && not (isSpace c) && c `notElem` s

featP :: ReadP (FeatureState, T.Text)
featP = do
    skipSpaces
    state <- choice [FPlus <$ char '+', FPlus <$ char '✓', FPlus <$ char '√', FMinus <$ char '-', FMinus <$ char '−', FOff <$ char '0'] <++ return FPlus
    feat <- (:) <$> satisfy (isPrintNelem "0123456789+-√−✓(){}[]¬^") <*> munch (isPrintNelem "(){}[]¬^")
    return (state, T.pack feat)

classP = between (char '[') (char ']') (NClass
     <$> ((True <$ char '¬') +++ (True <$ char '^') +++ return False)
     <*> many featP)

instance Read NaturalClass where
    readPrec = parens (lift (skipSpaces >> classP))



xor :: Bool -> Bool -> Bool
xor False p = p
xor True p = not p

-- | Convert a class to a 'SegSet'
classToSeglist :: FeatureTable sigma -> NaturalClass -> SegSet SegRef
classToSeglist ft (NClass isNegated cls) = force $ fnArray (srBounds ft) (\c -> isNegated `xor` and [ftlook ft c fi == fs | (fs,fi) <- icls])
    where icls = do
            (s,fn) <- cls
            Just fi <- return (M.lookup fn (featLookup ft))
            return (s,fi)

-- | Globs using 'NaturalClass' instead of 'SegSet'
data ClassGlob = ClassGlob {-# UNPACK #-} !Bool {-# UNPACK #-} !Bool [(GlobReps, NaturalClass)] deriving (Eq, Ord)
instance NFData ClassGlob where
    rnf (ClassGlob isinit isfin gparts) = isinit `seq` isfin `seq` rnf gparts

-- | Uses SPE format
instance Show ClassGlob where
    show (ClassGlob isinit isfin parts) = (guard isinit >> "#") ++ (showGP =<< parts) ++ (guard isfin >> "#") where
        showGP (GStar, NClass False []) = "…"
        showGP (rep, NClass False [(FPlus,"syllabic")]) = "V" ++ suf rep
        showGP (rep, NClass False [(FMinus,"syllabic")]) = "C" ++ suf rep
        showGP (rep, c) = show c ++ suf rep
        suf GSingle = ""
        suf GPlus = "₁"
        suf GStar = "₀"

globRepsP :: ReadP GlobReps
globRepsP = choice [GPlus <$ char '+', GPlus <$ char '₁', GStar <$ char '*', GStar <$ char '₀', return GSingle]

classGlobP :: ReadP ClassGlob
classGlobP = do
    isinit <- (True <$ char '#') +++ return False
    gparts <- many1 $ ((GStar, NClass False []) <$ char '…') +++ do
        cls <- classP +++ (NClass False [(FPlus,"syllabic")] <$ char 'V') +++ (NClass False [(FMinus,"syllabic")] <$ char 'C')
        rep <- globRepsP
        return (rep,cls)
    isfin <- (True <$ char '#') +++ return False
    return (ClassGlob isinit isfin gparts)

instance Read ClassGlob where
    readPrec = parens (lift (skipSpaces >> classGlobP))

-- | Convert to a 'ListGlob'
classesToLists :: FeatureTable sigma -> ClassGlob -> ListGlob SegRef
classesToLists ft (ClassGlob isinit isfin gparts) = ListGlob isinit isfin (fmap (second (classToSeglist ft)) gparts)

-- | Create a DFST which counts the matches of the glob.
cgMatchCounter :: FeatureTable sigma -> ClassGlob -> ShortDFST SegRef
cgMatchCounter ft = matchCounter . classesToLists ft
