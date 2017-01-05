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

runSVMProblem :: SVMProblem a  -> SVMParameters -> WorkingState -> ((a, String), WorkingState)
runSVMProblem prob params state = runState ( runReaderT (runWriterT prob ) params) state

type RawFeatures = [Double]

{-
 Try again... lets just build the high level stuff....

-}

makeParams :: Maybe SVMParameters -> SVMParameters
makeParams = error "implement makeParams"

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
  result <- pure $ mainLoop maxRounds
  newParams <- pure $ extractParams result
  pure $ runSVMProblem newParams (makeParams initParams) (WorkingState tsvList) ^. _1 . _1


{-| Extract the SVMParameters out of the transformer -}
extractParams :: SVMProblem a -> SVMProblem SVMParameters
extractParams = error "implement extractParams" -- ... maybe this should be runState?


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
solvePairs params (x:xs) = fst $ foldl (pairHelper params  (x:xs) ) ([], Just x) xs
solvePairs _ [] = []

{-| Helper function for solvePairs, it will run over the pairs and determine appropriate output -}
pairHelper :: SVMParameters
           -> [TrainingSupportVector]
           -> ([TrainingSupportVector], Maybe TrainingSupportVector)
           -> TrainingSupportVector
           -> ([TrainingSupportVector], Maybe TrainingSupportVector)
pairHelper _ _ (examined, Nothing) next =
  (examined, Just next)
pairHelper params allVecs (examined, Just target) next =
    case takeStepDetail params (WorkingState allVecs) target next  of
      Nothing -> ((next:examined), Just target)
      Just (r1, r2) -> (r1: r2:examined, Nothing)


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



-- Check to ensure that the Current vectors are not in the
inSet :: WorkingState -> TrainingSupportVector -> Bool
inSet _ _ = error "Implement inSet"

maybeInSet :: WorkingState -> TrainingSupportVector -> Maybe ()
maybeInSet ws sv | (inSet ws sv) == True = Just ()
maybeInSet _ _ = Nothing

maybePass :: Bool -> Maybe ()
maybePass True = Just ()
maybePass False = Nothing

testMaybe :: Bool -> Maybe ()
testMaybe True = Just ()
testMaybe False = Nothing

takeStepDetail :: SVMParameters
            -> WorkingState
            -> TrainingSupportVector
            -> TrainingSupportVector
            -> Maybe (TrainingSupportVector, TrainingSupportVector)
takeStepDetail params workState sv1 sv2 = --Just (sv1, sv2)
  let
        x1 = sv1^.supvec.vector
        x2 = sv2^.supvec.vector
        diff = sumVector (elementDifference x1 x2)
        identical = abs (diff) < params^.epsillon

  in
        do
            maybeInSet workState sv1
            maybeInSet workState sv2
            maybePass identical

            (a2, a2clip) <- determineAlpha2 params sv1 sv2
            a1 <- pure $ alpha1New sv1 sv2 (wrapScalar a2) (wrapScalar a2clip)

            sv1' <- pure $ SupportVector {_alpha= a1, _vector=sv1^.supvec.vector}
            sv2' <- pure $ SupportVector {_alpha=wrapScalar (a2), _vector=sv2^.supvec.vector}
            -- Since workState doesn't contain sv1 and sv2 we can append them to a temp list and use it here
            oldSvec <- pure $ focusOnSupportVector workState
            newSvec <- pure $ concat [[sv1', sv2'], oldSvec]

            pred1 <- pure $ svm params newSvec x1
            pred2 <- pure $ svm params newSvec x2
            finalSv1 <- pure $ constructTrainingVec sv1 pred1 sv1'
            finalSv2 <- pure $ constructTrainingVec sv2 pred2 sv2'

            return (finalSv1, finalSv2)-- modify existing training vector
