{-# LANGUAGE ScopedTypeVariables, ExplicitForAll #-}
module ConstraintLearner where

import Ring
import Probability
import WeightedDFA
import MaxentGrammar
import WeightOptimizer
import System.Random
import Control.Monad.State
import Control.DeepSeq
import Data.Ix
import Numeric
import Data.IORef
import System.IO
import Control.Exception


withHandler = flip catch

stopsigint :: AsyncException -> IO ()
stopsigint e = case e of
    UserInterrupt -> do
        putStrLn "\n\nInterrupted!"
        return ()
    _ -> throw e

-- main function to learn a list fo constraints,
generateGrammarIO :: forall g clabel sigma . (Show clabel, Ix sigma, NFData sigma, NFData clabel, Eq clabel) -- In the typical case, clabel = ClassGlob, sigma = SegRef
    => Int -- Monte Carlo sample size
    -> [Double] -- list of accuracy thresholds
    -> [(clabel, ShortDFST sigma)] -- list of constraints to try in order. Each constraint has a label and a dfa to compute it
    -> Lexicon sigma -- List of words to try and their frequencies
    -> IO ([clabel], MaxentViolationCounter sigma, Vec) -- computed grammar
generateGrammarIO samplesize thresholds candidates wfs = do
    let cbound = shortSegBounds . snd . head $ candidates
        blankdfa = nildfa cbound
        lendist = lengthCdf wfs
        pwfs = packMultiText cbound (wordFreqs wfs)

    hashctr :: IORef Int <- newIORef 0
    let mark500 = do
        c <- readIORef hashctr
        when (c `mod` 500 == 0) $ do
            hPutStr stderr "#"
            hFlush stderr
        modifyIORef' hashctr (+1)

    currentGrammar :: IORef ([clabel], MaxentViolationCounter sigma, Vec) <- newIORef ([],blankdfa,zero)

    let genSalad :: IO (PackedText sigma)
        genSalad = do
            (_,dfa,weights) <- readIORef currentGrammar
            salad' <- getStdRandom . runState $ sampleWordSalad (dropCounts (weightConstraints dfa weights)) lendist samplesize
            return . packMultiText cbound . wordFreqs . sortLexicon . fmap (\x -> (x,1)) $ salad'

    currentSalad <- newIORef undefined

    withHandler stopsigint $ do
        forM_ thresholds $ \accuracy -> do
            putStrLn $ "\n\n\nStarting pass with threshold " ++ showFFloat (Just 3) accuracy ""
            writeIORef currentSalad =<< genSalad
            forM_ candidates $ \(cl,cdfa) -> do
                mark500
                (grammar, dfa, weights) <- readIORef currentGrammar
                salad <- readIORef currentSalad
                let o = fromIntegral $ transducePacked cdfa pwfs
                    o' = fromIntegral $ transducePacked cdfa salad
                    e = o' * fromIntegral (totalWords wfs) / fromIntegral samplesize
                    score = upperConfidenceOE o e

                when (score < accuracy && cl `notElem` grammar) $ do
                    hPutStrLn stderr ""
                    putStrLn $ "\nSelected Constraint " ++ show cl ++  " (score=" ++ showFFloat (Just 4) score [] ++ ", o=" ++ showFFloat (Just 1) o [] ++ ", e=" ++ showFFloat (Just 1) e [] ++ ")."
                    let newgrammar = cl:grammar
                        newdfa = dfaProduct consMC (unpackShortDFST cdfa) dfa
                    putStrLn $ "New grammar has " ++ show (length grammar) ++ " constraints and " ++ (show . rangeSize . stateBounds $ newdfa) ++ " states."
                    let oldweights = consVec 0 weights
                    newweights <- evaluate . force $ llpOptimizeWeights wfs newdfa oldweights
                    hPutStrLn stderr ""
                    putStrLn $ "Recalculated weights: " ++ showFVec (Just 2) newweights
                    atomicWriteIORef currentGrammar . force $ (newgrammar, newdfa, newweights)
                    writeIORef currentSalad =<< genSalad
        putStrLn "\n\n\nAll Pases Complete."

    readIORef currentGrammar


{- main entry point for constraint learning.

This function makes no assumptions as to UG and instead, takes in a list of constraint candidates (in order of decreasing generality).
These constraints take the form of a pair consisting of a label (only compared for equality to prevent duplication)
and a DFA which counts the number of times a constraint is violated in a string.

To generate a constraint candidate list, several UG functions are contained in the PhonotacticGrammar module

generateGrammarIO :: (Ix sigma, Show clabel, Eq clabel, NFData sigma) => Int -- Monte Carlo sample size
    -> [Double] -- list of accuract thresholds
    -> [(clabel, ShortDFST sigma)] -- list of constraints to try in order. Each constraint has a label and a dfa to compute it
    -> Lexicon sigma -- List of words to try and their frequencies
    -> IO [(clabel, Double)]
generateGrammarIO samplesize thresholds (possibleConstraints) wfs = do
    ctr :: IORef Int <- newIORef 0
    let mark100 = do
        c <- readIORef ctr
        when (c `mod` 100 == 0) $ do
            putStr "#"
            hFlush stdout
        modifyIORef' ctr (+1)
    gen <- newStdGen
    evalStateT (generateGrammar (liftIO mark100) (liftIO . putStrLn) samplesize thresholds (possibleConstraints) wfs) gen
-}
