{-# LANGUAGE ScopedTypeVariables, ExplicitForAll, MultiParamTypeClasses, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Linguistics.PhonotacticLearner.UniversalGrammar where

import Linguistics.PhonotacticLearner.Util.Ring
import Linguistics.PhonotacticLearner.Util.Probability
import Linguistics.PhonotacticLearner.MaxentGrammar
import Linguistics.PhonotacticLearner.WeightedDFA

import Text.CSV
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
import Text.Read(Read(..),lift,parens)


-- enumeration for feature states (can be +,-,0)
data FeatureState = FOff | FPlus | FMinus deriving (Enum, Eq, Ord, Read, Show)
instance NFData FeatureState where
    rnf fs = fs `seq` ()

-- using integer indexes to a lookup table to represent segments
newtype SegRef = Seg Int deriving (Eq, Ord, Read, Show, Ix, NFData)

data FeatureTable sigma = FeatureTable { featTable :: Array (SegRef,Int) FeatureState
                                       , featNames :: Array Int T.Text
                                       , segNames :: Array SegRef sigma
                                       , featLookup :: M.Map T.Text Int
                                       , segLookup :: M.Map sigma SegRef } deriving (Show)

-- segment references
srBounds :: FeatureTable sigma -> (SegRef, SegRef)
srBounds ft = bounds (segNames ft)
{-# INLINE srBounds #-}

ftlook :: FeatureTable sigma -> SegRef -> Int -> FeatureState
ftlook ft sr fi = featTable ft ! (sr,fi)
{-# INLINE ftlook #-}

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



allFeatures :: FeatureTable sigma -> [T.Text]
allFeatures ft = elems (featNames ft)


--------------------------------------------------------------------------------


data NaturalClass = NClass { isInverted :: Bool
                           , featureList :: [(FeatureState, T.Text)]
                           } deriving (Eq, Ord)
instance NFData NaturalClass where
    rnf c@(NClass b fs) = b `seq` rnf fs

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

classToSeglist :: FeatureTable sigma -> NaturalClass -> SegSet SegRef
classToSeglist ft (NClass isNegated cls) = force $ fnArray (srBounds ft) (\c -> isNegated `xor` and [ftlook ft c fi == fs | (fs,fi) <- icls])
    where icls = do
            (s,fn) <- cls
            Just fi <- return (M.lookup fn (featLookup ft))
            return (s,fi)

-- for globs using feature classes instead of segment lists
data ClassGlob = ClassGlob Bool Bool [(GlobReps, NaturalClass)] deriving (Eq, Ord)
instance NFData ClassGlob where
    rnf (ClassGlob isinit isfin gparts) = isinit `seq` isfin `seq` rnf gparts

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

classesToLists :: FeatureTable sigma -> ClassGlob -> ListGlob SegRef
classesToLists ft (ClassGlob isinit isfin gparts) = ListGlob isinit isfin (fmap (second (classToSeglist ft)) gparts)

cgMatchCounter :: FeatureTable sigma -> ClassGlob -> ShortDFST SegRef
cgMatchCounter ft = matchCounter . classesToLists ft


ngrams  :: Int -> [a] -> [[a]]
ngrams  0  _       = [[]]
ngrams  _  []      = []
ngrams  n  (x:xs)  = fmap (x:) (ngrams (n-1) xs) ++ ngrams n xs





{-------------------------------------------------------------------------------

UNIVERSAL GRAMMAR FUNCTIONS

Put these together to generate lists of candidate constraints for the learner

-------------------------------------------------------------------------------}

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
