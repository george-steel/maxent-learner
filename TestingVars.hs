import MaxentGrammar
import Ring
import WeightedDFA
import MaxentGrammar
import WeightOptimizer

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Class
import System.Random
import Data.List
import Data.Monoid
import qualified Data.Map as M

abcLex = sortLexicon [ ("abc", 100)
                     , ("abcd", 100)
                     , ("acbc", 200)
                     , ("ccac", 500)
                     , ("aaba", 10)
                     , ("abb", 50)
                     , ("dbca", 200)
                     , ("bcd", 100)
                     ]

c1 = countngrams ('a','d') ["a","a"]
c2 = countngrams ('a','d') ["b","abd"]

g2 = dfaProduct consMC c2 (mapweights singleMC c1)

abcViols = observedViolations g2 abcLex

abcRandom :: Vec -> Int -> [String]
abcRandom weights seed = evalState (sequence . replicate 100 $ sampleWord dfa 4) (mkStdGen seed)
    where dfa = dropCounts (weightConstraints g2 weights)
