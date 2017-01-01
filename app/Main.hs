import Linguistics.PhonotacticLearner
import Linguistics.PhonotacticLearner.UniversalGrammar
import Linguistics.PhonotacticLearner.WeightedDFA

import Options.Applicative

data SegmentType = Chars | Words | Fierro deriving (Enum, Eq, Ord, Read, Show)

data Command = Learn {
        lexicon :: FilePath,
        hasFreqs :: Bool,
        useEdges :: Bool,
        useTrigrams :: Maybe String,
        useBroken :: Maybe String }
    | GenSalad {
        grammarfile :: FilePath }
    deriving Show

data ParsedArgs = ParsedArgs {
    ftable :: Maybe FilePath,
    segtype :: SegmentType,
    samplesize :: Int,
    outfile :: Maybe FilePath,
    cmd :: Command
} deriving (Show)

juststr = fmap Just str

parseOpts :: Parser ParsedArgs
parseOpts = ParsedArgs <$>
    option juststr (long "featuretable" <> short 't' <> metavar "CSVFILE" <> value Nothing <>
                    help "Use the features and segment list from a feature table in CSV format (a table for IPA is used by default).")
    <*> (flag' Chars (long "charsegs" <> short 'c' <> help "Use characters as segments (default).")
        <|> flag' Words (long "wordsegs" <> short 'w' <> help "Separate segments by spaces.")
        <|> flag' Fierro (long "fierrosegs" <> help "Parse segments by repeatedly taking the longest possible match and use ' to break up unintended digraphs (used for Fierro orthography).")
        <|> pure Chars)
    <*> option auto (long "samples" <> short 'n' <> value 3000 <> help "Number of samples to use for salad generation.")
    <*> (fmap Just (strOption $ long "output" <> short 'o' <> metavar "OUTFILE" <> help "Record final output to OUTFILE as well as stdout.") <|> pure Nothing)
    <*> subparser (command "learn" (info (Learn
            <$> strArgument (metavar "LEXICON")
            <*> switch (long "freqs" <> short 'f' <> help "Lexicon file contains word frequencies.")
            <*> switch (long "edges" <> short 'e' <> help "Allow constraints involving word boundaries")
            <*> option juststr (long "trigrams" <> short '3' <> metavar "COREFEATURES" <> value Nothing <>
                help "Allow trigram constraints where at least one class uses a single one of the following features (comma-separated)")
            <*> option juststr (long "repeats" <> short 'r' <> metavar "RUNCLASSES" <> value Nothing <>
                help "Allow constraints with two classes separated by a run of one of the so")
            ) (fullDesc <> progDesc "Learn a phonotactic grammar from a given lexicon"))
        <> command "gensalad" (info (GenSalad <$> strArgument (metavar "GRAMMAR"))
            (fullDesc <> progDesc "Generate random words from an already-calculated grammar")))

opts = info (helper <*> parseOpts) (fullDesc <> progDesc "Automatically infer phonotactic grammars from text and apply them as probability distributions.")

main = do
    args <- execParser opts
    putStrLn (show args)
