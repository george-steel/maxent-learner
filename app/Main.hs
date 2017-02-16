{-# LANGUAGE ParallelListComp, TemplateHaskell, ScopedTypeVariables #-}

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
import qualified Data.Map.Lazy as M
import Data.Array.IArray
import Data.Maybe
import Data.FileEmbed
import Data.Char
import Text.Read
import Numeric
import Control.Arrow
import System.Exit
import Control.DeepSeq
import Control.Exception
import Control.Parallel.Strategies
import System.Random

data SegmentType = Chars | Words | Fiero deriving (Enum, Eq, Ord, Read, Show)

data Command = Learn {
        lexicon :: FilePath,
        thresholds :: [Double],
        hasFreqs :: Bool,
        useEdges :: Bool,
        useTrigrams :: Maybe String,
        useBroken :: Maybe String }
    | GenSalad {
        grammarfile :: FilePath }
    deriving Show

data ParsedArgs = ParsedArgs {
    cmd :: Command,
    ftable :: Maybe FilePath,
    segtype :: SegmentType,
    samplesize :: Int,
    outfile :: Maybe FilePath
} deriving (Show)


parseOpts :: Parser ParsedArgs
parseOpts = ParsedArgs <$>
    hsubparser (command "learn" (info (Learn
            <$> strArgument (metavar "LEXICON")
            <*> option auto (long "thresholds" <> metavar "THRESHOLDS" <> value [0.01, 0.1, 0.2, 0.3] <> help "thresholds to use for candidate selection (default is [0.01, 0.1, 0.2, 0.3]).")
            <*> switch (long "freqs" <> short 'f' <> help "Lexicon file contains word frequencies.")
            <*> switch (long "edges" <> short 'e' <> help "Allow constraints involving word boundaries.")
            <*> optional (strOption $ long "trigrams" <> short '3' <> metavar "COREFEATURES" <>
                help "Allow trigram constraints where at least one class uses a single one of the following features (comma-separated).")
            <*> optional (strOption $ long "longdistance" <> short 'l' <> metavar "SKIPFEATURES" <>
                help "Allow constraints with two classes separated by a run of characters possibly restricted to all having one of the following features.")
            ) (fullDesc <> progDesc "Learn a phonotactic grammar from a given lexicon"))
        <> command "gensalad" (info (GenSalad <$> strArgument (metavar "GRAMMAR"))
            (fullDesc <> progDesc "Generate random words from an already-calculated grammar")))
    <*> optional (option str $ long "featuretable" <> short 't' <> metavar "CSVFILE" <>
        help "Use the features and segment list from a feature table in CSV format (a table for IPA is used by default).")
    <*> (flag' Chars (long "charsegs" <> short 'c' <> help "Use characters as segments (default).")
        <|> flag' Words (long "wordsegs" <> short 'w' <> help "Separate segments by spaces.")
        <|> flag' Fiero (long "fierosegs" <> help "Parse segments by repeatedly taking the longest possible match and use ' to break up unintended digraphs (used for Fiero orthography).")
        <|> pure Chars)
    <*> option auto (long "samples" <> short 'n' <> value 3000 <> help "Number of samples to use for salad generation.")
    <*> optional (strOption $ long "output" <> short 'o' <> metavar "OUTFILE" <> help "Record final output to OUTFILE as well as stdout.")

opts = info (helper <*> parseOpts) (fullDesc <> progDesc "Automatically infer phonotactic grammars from text and apply them as probability distributions.")




ipaft :: FeatureTable String
ipaft = fromJust (csvToFeatureTable id $(embedStringFile "./app/ft-ipa.csv"))

freqreader :: FeatureTable String -> (String -> [String]) -> String -> [([SegRef],Int)]
freqreader ft seg text = do
    line <- lines text
    let (wt@(_:_),wf') = break (== '\t') line
    [wf] <- return (words wf')
    Just n <- return $ readMaybe wf
    return (segsToRefs ft (seg wt), n)

nofreqreader :: FeatureTable String -> (String -> [String]) -> String -> [([SegRef],Int)]
nofreqreader ft seg text = do
    line <- lines text
    return (segsToRefs ft (seg line), 1)

prettyprintGrammar :: (Show clabel) => [clabel] -> Vec -> String
prettyprintGrammar grammar weights = (unlines . reverse) [showFFloat (Just 2) w "  " ++ show c | c <- grammar | w <- coords weights]

isNonComment :: String -> Bool
isNonComment [] = False
isNonComment "\n" = False
isNonComment ('#':_) = False
isNonComment _ = True

restrictedClasses :: FeatureTable String -> String -> [(NaturalClass, SegSet SegRef)]
restrictedClasses ft arg = fmap ((id &&& classToSeglist ft) . NClass False) $ [] : do
    feat <- fmap T.pack (words arg)
    Just _ <- return $ M.lookup feat (featLookup ft)
    [[(FPlus, feat)], [(FMinus, feat)]]


main = do
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
        Learn lexfile thresh lfreqs gedges gtris gbroken -> do
            let segmenter = case segtype args of
                    Words -> words
                    Chars -> fmap return
                    Fiero -> segmentFiero (elems (segNames ft))
                cls = force $ classesByGenerality ft 3
            lexdata <- readFile lexfile
            let lexlist = (if lfreqs then freqreader else nofreqreader) ft segmenter lexdata
            when (null lexlist) (die "Invalid lexicon file")
            let wfs = sortLexicon lexlist
                singles = ugSingleClasses cls
                edges = if gedges then (ugEdgeClasses cls) else []
                doubles = ugBigrams cls
                edoubles = if gedges then (ugEdgeBigrams cls) else []
                triples = case gtris of Just rcls -> ugLimitedTrigrams cls (restrictedClasses ft rcls)
                                        Nothing -> []
                longdistance = case gbroken of Just rcls -> ugLongDistance cls (restrictedClasses ft rcls)
                                               Nothing -> []

            globs <- evaluate . force $ join [singles,edges,doubles,edoubles,triples,longdistance]
            putStrLn $ "Generated candidates with " ++ show (length cls) ++ " classes and " ++ show (length globs) ++ " globs, running DFA generation in parallel."
            let candidates = fmap (force . (id *** matchCounter)) globs `using` (parListChunk 1000 rdeepseq)

            (grammar, dfa, weights) <- generateGrammarIO (samplesize args) thresh candidates lexlist

            let output = "# Length Distribution:\n" ++ (show . assocs . lengthFreqs $ wfs) ++ "\n\n# Rules:\n" ++ prettyprintGrammar grammar weights

            putStrLn "\n\n\n\n"
            putStrLn output

            case outfile args of
                Just outf -> writeFile outf output
                Nothing -> return ()



        GenSalad gfile -> do
            rawgrammar <- readFile gfile

            (fline:glines) <- evaluate $ filter isNonComment (lines rawgrammar)

            let lendist :: [(Int,Int)] = read fline
                grammar :: [(Double,ClassGlob)] = fmap ((read *** read) . break isSpace) glines
                lencdf = massToCdf (fmap (second fromIntegral) lendist)
                (weightlist,rulelist) = unzip (reverse grammar)
                weights = vec weightlist
                blankdfa = nildfa (srBounds ft)
                dfa = foldr (\g t -> force $ dfaProduct consMC (unpackDFA . cgMatchCounter ft $ g) (force t)) blankdfa rulelist
                unsegmenter = case segtype args of
                    Words -> unwords
                    Chars -> join
                    Fiero -> joinFiero (elems (segNames ft))

            evaluate . force $ grammar
            evaluate . force $ dfa

            salad <- getStdRandom . runState $ sampleWordSalad (fmap (maxentProb weights) dfa) lencdf (samplesize args)

            let output = unlines . fmap (unsegmenter . refsToSegs ft) $ salad

            putStrLn "\n\n\n\n"
            putStrLn output

            case outfile args of
                Just outf -> writeFile outf output
                Nothing -> return ()

            return ()
