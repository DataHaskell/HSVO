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
  svmProblem <- pure $ constructProblem initParams tsvList
  result <- pure $ mainLoop =<< svmProblem
  pure $ error "extract the SVMParameters out of the result"
  Nothing

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
  workState <- get
  tsvList <- pure $  workState ^.. (vectorList)
  pure $ error "try and work out how to decompose the next step.. we need to iterate over the list once!!"
  return False

-- Note to self, we could use the lens "zoom" to implement the reduced set heuristic
-- from the paper....

{-| Construct a SVM Problem monad -}
constructProblem :: Maybe SVMParameters -> [TrainingSupportVector] -> SVMProblem a
constructProblem = error "implement constructProblem"

{-| Construct a list of supportVector objects from the raw data  -}
makeSupportVectors :: [RawFeatures] -> [ClassLabel] -> Maybe [TrainingSupportVector]
makeSupportVectors = error "implement makeSupportVectors"

{-| HighLevel function to predict data given an SVM -}
predictSVM :: SVMParameters -> [RawFeatures] -> [ClassLabel]
predictSVM params testData = error "implement predictSVM"
