{-# LANGUAGE ParallelListComp, TemplateHaskell, ScopedTypeVariables, DoAndIfThenElse #-}

{-
Command line interface for phonotactic learner
Copyright © 2016-2017 George Steel and Peter Jurgec

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
-}

import Text.PhonotacticLearner
import Text.PhonotacticLearner.PhonotacticConstraints
import Text.PhonotacticLearner.PhonotacticConstraints.FileFormats
import Text.PhonotacticLearner.PhonotacticConstraints.Generators
import Text.PhonotacticLearner.DFST
import Text.PhonotacticLearner.Util.Ring
import Text.PhonotacticLearner.Util.Probability
import Text.PhonotacticLearner.MaxentGrammar

import Options.Applicative
import Options.Applicative.Extra
import Control.Monad
import Control.Monad.State
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.ByteString as B
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Data.Monoid
import Data.Array.IArray
import Data.Maybe
import Data.FileEmbed
import Data.Char
import Data.Foldable
import Text.Read
import Numeric
import Control.Arrow
import System.Exit
import Control.DeepSeq
import Control.Exception
import Control.Parallel.Strategies
import System.Random
import System.IO
import System.Random.Shuffle

data SaladOutput = SpacedLex | FieroLex | FieroShuffled deriving (Eq, Ord, Read, Show, Enum)

data Command = Learn
        FilePath -- Lexicon file
        Bool -- Collate lexicon
        [Double] -- Thresholds for learner
        Bool -- Use edges
        (Maybe String) -- Trigram cores
        (Maybe String) -- Long distance centers
    | GenSalad
        FilePath -- Grammar file
        Bool -- space outputs
        Bool  -- shuffle outputs
    deriving Show

data ParsedArgs = ParsedArgs {
    cmd :: Command,
    ftable :: Maybe FilePath,
    samplesize :: Int,
    outfile :: Maybe FilePath
} deriving (Show)


parseOpts :: Parser ParsedArgs
parseOpts = ParsedArgs <$>
    hsubparser (command "learn" (info (Learn
            <$> strArgument (metavar "LEXICON")
            <*> switch (long "collate" <> short 'c' <> help "Collate a lexicon from raw text, spaces separate words. When this is off, one ")
            <*> option auto (long "thresholds" <> metavar "THRESHOLDS" <> value [0.01, 0.1, 0.2, 0.3] <> help "thresholds to use for candidate selection (default is [0.01, 0.1, 0.2, 0.3]).")
            <*> switch (long "edges" <> short 'e' <> help "Allow single classes and bigrams restricted to word boundaries.")
            <*> optional (strOption $ long "trigrams" <> short '3' <> metavar "COREFEATURES" <>
                help "Allows trigrams as long as at least one class is [] or [±x] where x is in COREFEATURES (space separated in quotes).")
            <*> optional (strOption $ long "longdistance" <> short 'l' <> metavar "SKIPFEATURES" <>
                help "Allows long-distance constraints of the form AB+C where A,C are classes and C = [] or [±x] with x in SKIPFEATURES.")
            ) (fullDesc <> progDesc "Learn a phonotactic grammar from a given lexicon"))
        <> command "gibber" (info (GenSalad
            <$> strArgument (metavar "GRAMMAR")
            <*> switch (long "spaced" <> help "Separate segments with spaces")
            <*> switch (long "shuffle" <> short 's' <> help "Shuffle generate output (sorted by default)")
            ) (fullDesc <> progDesc "Generate a salad of random words from an already-calculated grammar")))
    <*> optional (option str $ long "featuretable" <> short 'f' <> metavar "CSVFILE" <>
        help "Use the features and segment list from a feature table in CSV format (a table for IPA is used by default).")
    <*> option auto (long "samples" <> short 'n' <> value 3000 <> help "Number of samples to use for salad generation.")
    <*> optional (strOption $ long "output" <> short 'o' <> metavar "OUTFILE" <> help "Record final output to OUTFILE as well as stdout.")

opts = info (helper <*> parseOpts) (fullDesc <> progDesc "Automatically infer phonotactic grammars from text and apply them as probability distributions.")


ipaft :: FeatureTable String
ipaft = fromJust (csvToFeatureTable id $(embedStringFile "./app/ft-ipa.csv"))

main = do
    hSetEncoding stdout utf8
    args <- execParser opts
    putStrLn (show args)
    ft <- case ftable args of
        Just fname -> do
            ftcsv <- readFile fname
            case csvToFeatureTable id ftcsv of
                Just ft -> return ft
                Nothing -> die "Invalid feature table."
        Nothing -> do
            putStrLn "Using default IPA feature table."
            return ipaft

    case cmd args of
        Learn lexfile docollate thresh edges tris broken -> do
            rawlex <- fmap (T.decodeUtf8With T.lenientDecode) (B.readFile lexfile)
            let segs = S.fromList . elems . segNames $ ft
                parsedLex = (if docollate then collateWordlist else parseWordlist) segs rawlex
                candsettings = CandidateSettings edges (fmap (T.words . T.pack) tris) (fmap (T.words . T.pack) broken)
                cookedlex = fmap (\(LexRow w f) -> (segsToRefs ft w, f)) parsedLex
            when (null cookedlex) (die "Invalid lexicon file")
            (ncls, nglobs, globs) <- evaluate $ candidateGrammar ft candsettings
            putStrLn $ "Generated candidates with " ++ show ncls ++ " classes and " ++ show nglobs ++ " globs, running DFA generation in parallel."

            let candidates = fmap (force . (id *** matchCounter)) globs `using` (parListChunk 1000 rdeepseq)
            (lenarr, grammar, dfa, weights) <- generateGrammarIO (samplesize args) thresh candidates cookedlex

            let output = serGrammar (PhonoGrammar lenarr grammar weights)
            putStrLn "\n\n\n\n"
            T.putStrLn output

            case outfile args of
                Just outf -> B.writeFile outf (T.encodeUtf8 output)
                Nothing -> return ()



        GenSalad gfile dospace doshuffle -> do
            rawgrammar <- fmap (T.decodeUtf8With T.lenientDecode) (B.readFile gfile)
            Just (PhonoGrammar lendist rules ws) <- evaluate . force $ parseGrammar rawgrammar

            let nrules = length rules
                lencdf = massToCdf (fmap (second fromIntegral) (assocs lendist))
                blankdfa :: MulticountDFST SegRef
                blankdfa = pruneAndPack . nildfa $ srBounds ft
                addRule :: ClassGlob -> MulticountDFST SegRef -> IO (MulticountDFST SegRef)
                addRule r g = do
                    g' <- evaluate . pruneAndPack $ rawIntersection consMC (unpackDFA . cgMatchCounter ft $ r) (unpackDFA g)
                    hPutStr stderr "#" >> hFlush stderr
                    return g'

            dfa <- foldrM addRule blankdfa rules
            hPutStrLn stderr "\n" >> hFlush stderr
            salad <- getStdRandom . runState $ sampleWordSalad (fmap (maxentProb ws) (unpackDFA dfa)) lencdf (samplesize args)

            let segs = S.fromList . elems . segNames $ ft
            output <- if doshuffle then do
                salad' <- fmap (shuffle' salad (length salad)) newStdGen
                let saladsegs = fmap (refsToSegs ft) salad'
                    unseg = if dospace then unwords else joinFiero segs
                return . T.unlines . fmap (T.pack . unseg) $ saladsegs
            else return $ let
                sortedsalad = wordFreqs . sortLexicon . fmap (\x -> (x,1)) $ salad
                in (if dospace then serWordlistSpaced else serWordlist segs) . fmap (\(w,f) -> LexRow (refsToSegs ft w) f) $ sortedsalad

            putStrLn "\n\n\n\n"
            T.putStrLn output

            case outfile args of
                Just outf -> B.writeFile outf (T.encodeUtf8 output)
                Nothing -> return ()

            return ()
