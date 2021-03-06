{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ParallelListComp #-}

{-|
Module: Text.PhonotacticLearner.PhonotacticConstraints.FileFormats
Description: Generation of candidate constraint sets.
Copyright: © 2016-2017 George Steel and Peter Jurgec
License: GPL-2+
Maintainer: george.steel@gmail.com

Functions for saving and loading lexicons and 'ClassGlob' constraint grammars in standard formats.
-}

module Text.PhonotacticLearner.PhonotacticConstraints.FileFormats where

import Control.Monad
import Data.Traversable
import Data.Monoid
import Data.Foldable
import Text.Read
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Array.IArray
import Numeric
import Control.DeepSeq

import Text.PhonotacticLearner.PhonotacticConstraints
import Text.PhonotacticLearner.MaxentGrammar
import Text.PhonotacticLearner.Util.Ring


-- | Given a set of possible segments and a string, break a string into segments.
-- Uses the rules in Fiero orthography (a phonetic writing system using ASCII characters) where the longest possible match is always taken and apostrophes are used as a digraph break.
segmentFiero :: S.Set String -- ^ All possible segments
             -> String -- ^ Raw text
             -> [String] -- ^ Segmented text
--segmentFiero [] = error "Empty segment list."
segmentFiero allsegs = go msl where
    msl = maximum . S.map length $ allsegs
    go _ [] = []
    go _ ('\'':xs) = go msl xs
    go 0 (x:xs) = go msl xs
    go len xs | S.member seg allsegs = seg : go msl rest
              | otherwise = go (len-1) xs
        where (seg,rest) = splitAt len xs

-- | Joins segments together using Fiero rules. Inserts apostrophes where necerssary.
joinFiero :: S.Set String -- ^ All possible segments
          -> [String] -- ^ Segmented text
          -> String -- ^ Raw text
joinFiero allsegs = go where
    msl = maximum . S.map length $ allsegs
    go [] = []
    go [x] = x
    go (x:xs@(y:_)) = let z = x++y
                      in  if any (\s -> isPrefixOf s z && not (isPrefixOf s x)) allsegs
                          then x ++ ('\'' : go xs)
                          else x ++ go xs

-- | Structure for reperesenting lexicon entries
data LexRow = LexRow [String] Int

-- | Parse a lexicon from a file. Segmentation of a word uses fiero rules (which will also decode space-separated segments and single-character segments).
-- Words may optionally be followed by a tab character and an integer indicating frequency (1 by default).
parseWordlist :: S.Set String -> T.Text -> [LexRow]
parseWordlist segs rawlist = do
    line <- T.lines rawlist
    let (rawword : rest) = T.split (== '\t') line
        w = segmentFiero segs (T.unpack rawword)
        fr = fromMaybe 1 $ do
            [f] <- return (rest >>= T.words)
            readMaybe (T.unpack f)
    guard (w /= [])
    return $ LexRow w fr

-- | Collate a list of words and frequencies from raw phonetic text.
collateWordlist :: S.Set String -> T.Text -> [LexRow]
collateWordlist segs rawtext = fmap (uncurry LexRow) . M.assocs . M.fromListWith (+) $ do
    rawword <- T.words rawtext
    let w = segmentFiero segs (T.unpack rawword)
    guard (w /= [])
    return (w, 1)

-- | Serializes a list of words and frequerncies to a string for decoding with 'parseWordlist'. Connects segments using Fiero rules.
serWordlist :: S.Set String -> [LexRow] -> T.Text
serWordlist segs = T.unlines . mapMaybe showRow where
    showRow (LexRow _ n) | n <= 0 = Nothing
    showRow (LexRow w 1) = Just . T.pack $ joinFiero segs w
    showRow (LexRow w n) = Just . T.pack $ joinFiero segs w ++ "\t" ++ show n

-- | Serializes a list of words and frequerncies to a string for decoding with 'parseWordlist'. Puts spaces between segments.
serWordlistSpaced :: [LexRow] -> T.Text
serWordlistSpaced = T.unlines . mapMaybe showRow where
    showRow (LexRow _ n) | n <= 0 = Nothing
    showRow (LexRow w 1) = Just . T.pack $ unwords w
    showRow (LexRow w n) = Just . T.pack $ unwords w ++ "\t" ++ show n

-- | Reperesentation of a 'ClassGlob' grammar.
data PhonoGrammar = PhonoGrammar {
    lengthDist :: (Array Length Int), -- ^ Distribution of word lengths
    constraintSet :: [ClassGlob], -- ^ Set of constraints
    weightSet :: Vec -- ^ Set of weights in same order as constraints
} deriving (Eq, Show)

instance NFData PhonoGrammar where
    rnf (PhonoGrammar lendist grammar weights) = rnf lendist `seq` rnf grammar `seq` rnf weights

-- | Parse a grammar from a file. Blank lines ans lines begining with # are ignored.
-- The first regular line must contain a list of (Length,Int) pairs and subsequent lines must contain a weight followed by a ClassGlob.
parseGrammar :: T.Text -> Maybe PhonoGrammar
parseGrammar rawgrammar = do
    let noncomment l = not (T.null l) && (T.head l /= '#')
    (fline:glines) <- return $ filter noncomment (T.lines rawgrammar)
    lenlist <- readMaybe (T.unpack fline)
    let maxlen = maximum (fmap fst lenlist)
        lenarr = accumArray (+) 0 (1,maxlen) (filter ((> 0) . fst) lenlist)
        readline l = do
            let (wt, ct') = T.breakOn " " l
            w::Double <- readMaybe (T.unpack wt)
            (' ', ct) <- T.uncons ct'
            c::ClassGlob <- readMaybe (T.unpack ct)
            return (c,w)
    cs <- traverse readline (reverse glines)
    return $ PhonoGrammar lenarr (fmap fst cs) (vec (fmap snd cs))

-- | Serialize a grammar without length distribution
serGrammarRules :: [ClassGlob] -> Vec -> T.Text
serGrammarRules grammar weights =
    (T.unlines . reverse) [T.pack $ showFFloat (Just 3) w "  " ++ show c | c <- grammar | w <- coords weights]

-- | Serialize a grammar including length distribution
serGrammar :: PhonoGrammar -> T.Text
serGrammar (PhonoGrammar lendist grammar weights) =
    "# Length Distribution:\n" <> (T.pack . show . assocs $ lendist) <> "\n\n# Constraints:\n" <> serGrammarRules grammar weights
