{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
import Ring
import Probability
import WeightedDFA
import MaxentGrammar
import WeightOptimizer
import PhonotacticGrammar
import ConstraintLearner

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Class
import System.Random
import Data.List
import Data.Monoid
import Data.Maybe
import qualified Data.Map as M
import Control.Arrow
import Text.Read
import Control.Exception (evaluate)
import Control.DeepSeq

import Control.Parallel.Strategies
import Data.FileEmbed

ftcsv :: String
ftcsv = $(embedStringFile "./features-char.csv")

onsetcsv :: String
onsetcsv = $(embedStringFile "./testcases/features-onsets.csv")

ipaft :: FeatureTable Char
ipaft = fromJust (csvToFeatureTable head ftcsv)

onsetft :: FeatureTable String
onsetft = fromJust (csvToFeatureTable id onsetcsv)

onsetLex = sortLexicon $ do
    line <- lines $(embedStringFile "./testcases/EnglishLearningData.txt")
    let (sw,sn') = break (== '\t') line
    (_:sn) <- return sn'
    Just n <- return $ readMaybe sn
    return (segsToRefs onsetft (words sw), n)

shonaFT = fromJust (csvToFeatureTable id $(embedStringFile "./testcases/ShonaFeatures.csv"))
shonaLex = sortLexicon $ do
    line <- lines $(embedStringFile "./testcases/ShonaLearningData.txt")
    let (sw,sn') = break (== '\t') line
    [sn] <- return (words sn')
    Just n <- return $ readMaybe sn
    return (segsToRefs shonaFT (words sw), n)
{-
abcLex = sortLexicon [ ("abc", 1000)
                     , ("abcd", 1000)
                     , ("acbc", 2000)
                     , ("ccac", 5000)
                     --, ("aaba", 10)
                     , ("abb", 500)
                     , ("dbca", 2000)
                     , ("bcd", 1000)
                     , ("dcdc", 100)
                     ]
abcLen = lengthCdf abcLex

c1 = countngrams ('a','d') ["a","a"]
c2 = countngrams ('a','d') ["b","abd"]

abcCandidates = fmap (id &&& countngrams ('a','d')) [["a","a"], ["c","c"],["b","abd"], ["d","ac"]]

g2 = dfaProduct consMC c2 (fmap singleMC c1)

abcViols = observedViolations g2 abcLex

abcRandom :: Vec -> Int -> [String]
abcRandom weights seed = evalState (sampleWordSalad dfa abcLen 100) (mkStdGen seed)
    where dfa = dropCounts (weightConstraints g2 weights)

onsetClasses = classesByGenerality onsetft 3

onsetCoreClasses = fmap (NClass False . return) [(FPlus,"consonantal"),
                                                 (FMinus,"consonantal"),
                                                 (FPlus,"sonorant"),
                                                 (FMinus,"sonorant")]

onsetCandidates = fmap (id &&& cgMatchCounter onsetft) $ ugMiddleHayesWilson onsetClasses onsetCoreClasses
-}

shonaClasses = classesByGenerality shonaFT 3
shonaCoreClasses = fmap ((id &&& classToSeglist shonaFT) . NClass False) [[],
                                        [(FPlus,"syllabic")],
                                        [(FMinus,"syllabic")],
                                        [(FPlus,"consonantal")],
                                        [(FMinus,"consonantal")],
                                        [(FPlus,"sonorant")],
                                        [(FMinus,"sonorant")]]

main = do
    evaluate $ force shonaClasses
    putStrLn $ "Generating grammar using " ++ show (length shonaClasses) ++ " classes."
    let shonaGlobs = ugMiddleHayesWilson shonaClasses shonaCoreClasses
    evaluate $ force shonaGlobs
    putStrLn $ "Generated " ++ show (length shonaGlobs) ++ " globs, computing DFAs."
    let shonaCandidates = fmap (force . (id *** matchCounter (srBounds shonaFT))) shonaGlobs `using` (parListChunk 1000 rdeepseq)
    evaluate $ force shonaCandidates
    putStrLn $ "Computed UG."
    grammar <- generateGrammarIO 3000 [0.01, 0.1, 0.2, 0.3] shonaCandidates shonaLex
    putStrLn . unlines . fmap show $ grammar
