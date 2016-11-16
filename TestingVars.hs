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

import Data.FileEmbed

ftcsv :: String
ftcsv = $(embedStringFile "./features-char.csv")

ipaft :: FeatureTable Char
ipaft = fromJust (csvToFeatureTable head ftcsv)

abcLex = sortLexicon [ ("abc", 100)
                     , ("abcd", 100)
                     , ("acbc", 200)
                     , ("ccac", 500)
                     , ("aaba", 10)
                     , ("abb", 50)
                     , ("dbca", 200)
                     , ("bcd", 100)
                     , ("dcdc", 10)
                     ]
abcLen = lengthCdf abcLex

c1 = countngrams ('a','d') ["a","a"]
c2 = countngrams ('a','d') ["b","abd"]

abcCandidates = fmap (id &&& countngrams ('a','d')) [["a","a"], ["c","c"],["b","abd"], ["d","ac"]]

g2 = dfaProduct consMC c2 (mapweights singleMC c1)

abcViols = observedViolations g2 abcLex

abcRandom :: Vec -> Int -> [String]
abcRandom weights seed = evalState (sampleWordSalad dfa abcLen 100) (mkStdGen seed)
    where dfa = dropCounts (weightConstraints g2 weights)
