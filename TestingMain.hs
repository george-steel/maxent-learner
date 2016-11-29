{-# LANGUAGE TemplateHaskell #-}
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
                                                 (FMinus,"sonorantsonorant")]

onsetCandidates = fmap (id &&& cgMatchCounter onsetft) $ localBigramGlobs onsetClasses onsetCoreClasses

main = do
    evaluate $ force onsetCandidates
    putStrLn "Computed UG."
    grammar <- generateGrammarIO 3000 [0.001, 0.01, 0.1, 0.2, 0.3] onsetCandidates onsetLex
    putStrLn . unlines . fmap show $ grammar
