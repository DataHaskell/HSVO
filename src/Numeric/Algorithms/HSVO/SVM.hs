{- SVM main implementation area -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Numeric.Algorithms.HSVO.SVM where


import Numeric.Algorithms.HSVO.Detail
import Numeric.Algorithms.HSVO.Types

import Control.Lens
import Data.Maybe (catMaybes)
import qualified Data.Vector as V
import Control.Monad.Reader --(MonadReader, Reader, ReaderT)
import Control.Monad.State --(MonadState, State, StateT)
import Control.Monad.Writer

type SVMProblem a = WriterT String (ReaderT SVMParameters (State WorkingState)) a

type RawFeatures = [Double]

{-
 Try again... lets just build the high level stuff....

-}


{-| HighLevel function to fit an SVM, takes in the training data and labels -}
fitSVM' :: [RawFeatures] -> [ClassLabel] -> Maybe SVMParameters -> Maybe SVMParameters
fitSVM' trainingData trainingLabels initParams = do
  supVectors <- makeSupportVectors trainingData trainingLabels
  fitSVM initParams supVectors

{-| Given a list of support vectors, solve to find the parameters -}
fitSVM :: Maybe SVMParameters -> [TrainingSupportVector] -> Maybe SVMParameters
fitSVM initParams tsvList = do
  maxRounds <- case initParams of
                Nothing -> pure 100
                Just params -> pure $ params ^. maxIters
  svmProblem <- pure $ constructProblem initParams tsvList maxRounds
  result <- pure $ mainLoop =<< svmProblem
  pure $ extractParams result


{-| Extract the SVMParameters out of the transformer -}
extractParams :: SVMProblem a -> SVMParameters
extractParams = error "implement extractParams"


{-| The actual main loop. Implemented as a recursive function. -}
mainLoop :: Int -> SVMProblem Bool
mainLoop remainingIters
  | remainingIters == 0 = return False
  | otherwise = do
      converged <- takeStep
      if converged then return True else mainLoop (remainingIters - 1 )

{-| take a single step of the algorithm -}
takeStep :: SVMProblem Bool
takeStep = do
  --workState <- get
  params <- ask
  shuffleVectors
  vectorList %= solvePairs params

  error "determine convergence"
  return False


{-| Shuffle vectors -}
shuffleVectors :: SVMProblem ()
shuffleVectors = error "Implemnet shuffleVectors"

{-| Walk the list and then attempt to improve the SVM. Don't forget to shuffle the list! -}
solvePairs :: SVMParameters -> [TrainingSupportVector] -> [TrainingSupportVector]
solvePairs params (x:xs) = fst $ foldl (pairHelper params (x:xs)) ([], Just x) xs
solvePairs _ [] = []

{-| Helper function for solvePairs, it will run over the pairs and determine appropriate output -}
pairHelper :: SVMParameters -> [TrainingSupportVector]
           -> ([TrainingSupportVector], Maybe TrainingSupportVector)
           -> TrainingSupportVector
           -> ([TrainingSupportVector], Maybe TrainingSupportVector)
pairHelper _ _ (examined, Nothing) next =
  (examined, Just next)
pairHelper params allVecs (examined, Just target) next =
  let
    result = optimizePair params allVecs target next
  in
    case result of
      Nothing -> ((next:examined), Just target)
      Just (r1, r2) -> (r1: r2:examined, Nothing)


{-| Now we have a pair of support vectors, lets optimise them. Note that the two that are selected
will also be present in the allVecs list -}
optimizePair :: SVMParameters
             -> [TrainingSupportVector]
             -> TrainingSupportVector
             -> TrainingSupportVector
             -> Maybe (TrainingSupportVector, TrainingSupportVector)
optimizePair params allVecs t1 t2  = error "implement optimizePair"


-- Note to self, we could use the lens "zoom" to implement the reduced set heuristic
-- from the paper....
-- Also, the order of the training examples is arbitrary... I just want to choose two
-- and work with them, then choose a different two untill the list is exhausted...


{-| Construct a SVM Problem monad -}
constructProblem :: Maybe SVMParameters -> [TrainingSupportVector] -> Int -> SVMProblem Int
constructProblem = error "implement constructProblem"

{-| Construct a list of supportVector objects from the raw data  -}
makeSupportVectors :: [RawFeatures] -> [ClassLabel] -> Maybe [TrainingSupportVector]
makeSupportVectors = error "implement makeSupportVectors"

{-| HighLevel function to predict data given an SVM -}
predictSVM :: SVMParameters -> [RawFeatures] -> [ClassLabel]
predictSVM params testData = error "implement predictSVM"
