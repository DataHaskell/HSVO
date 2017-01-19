{- SVM main implementation area -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Numeric.Algorithms.HSVO.SVM where


import Numeric.Algorithms.HSVO.Detail
import Numeric.Algorithms.HSVO.Types

import Control.Lens
import Data.Maybe (catMaybes)
import qualified Data.Vector as V
import Control.Monad.Reader --(MonadReader, Reader, ReaderT)
import Control.Monad.State --(MonadState, State, StateT)
import Control.Monad.Writer
import Control.Monad.Random
import System.Random
import System.Random.Shuffle

type SVMProblem a = WriterT String (ReaderT SVMParameters (StateT WorkingState (Rand StdGen))) a

runSVMProblem :: SVMProblem a  -> SVMParameters -> WorkingState -> (((a, String), WorkingState), StdGen)
runSVMProblem prob params state = runRand (runStateT ( runReaderT (runWriterT prob ) params) state) (mkStdGen 10)

type RawFeatures = [Double]

makeParams :: Maybe SVMParameters -> SVMParameters
makeParams (Just p) = p
makeParams Nothing = testParams

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
  newParams <- pure $ (extractParams . mainLoop) maxRounds
  runSVMProblem newParams (makeParams initParams) (WorkingState tsvList) ^. _1 ._1 . _1


{-| Extract the SVMParameters out of the transformer -}
extractParams :: SVMProblem Bool -> SVMProblem (Maybe SVMParameters)
extractParams m = do
  test <- m
  workState <- get
  params <- ask
  supVecs <- pure $ (filterSupVectors params) $ focusOnSupportVector workState
  if not test
    then pure Nothing
    else pure $ Just params

{-| Remove any support vectors whose weight is zero from the list -}
filterSupVectors :: SVMParameters -> [SupportVector] -> [SupportVector]
filterSupVectors params = filter (filterVec params)

{-| Determine if a supVector should be filtered out -}
filterVec :: SVMParameters -> SupportVector -> Bool
filterVec params sv = if abs (sv ^. alpha) >= params ^. epsillon then True else False

{- Wrapper on filterVec to take training support vectors -}

filterTSV :: SVMParameters -> TrainingSupportVector -> Bool
filterTSV params tsv = filterVec params $ tsv ^. supvec
-- To create a law-abiding filter, lets apply the filter, remember the indecies, and then use the indecies to construct the lens. This way we filter what we want, but don't break any laws... although we suffer a penalty... an alternative would be to make this better by changing it


{-| A function that takes a working state and produces a filtered lens.
  -}
--makeFilter :: SVMParameters -> WorkingState -> Optical' [TrainingSupportVector] (Indexed Int) f a a
--IndexedFold Int WorkingState [TrainingSupportVector]

helpLens
  :: (Indexable Int p, Applicative f) =>
     p SupportVector (f SupportVector) -> WorkingState -> f WorkingState
helpLens = vectorList . traversed <. supvec


{-makeFilter :: (Indexable Int p, Applicative f) =>
     SVMParameters
     -> WorkingState
     -> p SupportVector (f SupportVector)
     -> WorkingState
     -> f WorkingState -}
makeFilter :: SVMParameters -> WorkingState -> Traversal' WorkingState TrainingSupportVector
makeFilter params ws =
  let
    indexedSupVec = vectorList . itraversed  --   :: (Indexable Int p, Applicative f) => p SupportVector (f SupportVector) -> WorkingState -> f WorkingState
    targetIndicies = ifoldMapOf indexedSupVec
      (\i a -> if filterTSV params a then [i] else []) ws
    filt = ifiltered (\i _ -> i `elem` targetIndicies)
  in
    (vectorList . traversed ) . filt


{-| The actual main loop. Implemented as a recursive function. -}
mainLoop :: Int -> SVMProblem Bool
mainLoop remainingIters
  | remainingIters == 0 = return False
  | otherwise = do
      converged <- takeStep
      if converged then return True else mainLoop (remainingIters - 1 )


{-| Evaluate the SVM's current error -}
evalSVM :: SVMProblem Double
evalSVM = do
  ws <- get
  params <- ask
  let
    svList =  ws ^.. vectorList . traversed. supvec
    samples = ws ^.. vectorList . traversed . supvec . vector
    trueLabels = ws ^.. vectorList . traversed . trueLabel
    f = svm params svList
    predictedLabels = map f samples
    truePredPairs = zip trueLabels predictedLabels
    modelError = foldl (\b (t, p)  -> b + calcClassError t p ) 0.0 truePredPairs
  pure modelError


{-| take a single step of the algorithm -}
takeStep :: SVMProblem Bool
takeStep = do
  --workState <- get
  params <- ask
  shuffleVectors
  ws <- get
  trainError <- evalSVM

  (newVector, intResult) <- pure $ solvePairs params $ filter (filterTSV params) $ ws ^. vectorList
  vectorList .= newVector

  result <-if intResult
           then pure intResult
           else do
                 (newPairsComplete, completeResult) <- pure $ solvePairs params $ ws ^. vectorList
                 vectorList .= newVector
                 pure completeResult

  newTrainError <- evalSVM

  -- Consider converged if our the difference in our error is too small.
  if abs (newTrainError - trainError) <= params ^. epsillon then pure False else pure result

{-| Shuffle vectors -}
shuffleVectors :: SVMProblem ()
shuffleVectors = do
  ws <- get :: SVMProblem WorkingState
  let
    l = ws ^.. vectorList . traversed :: [TrainingSupportVector]
  shuffled <- (shuffleM l) :: SVMProblem [TrainingSupportVector]
  vectorList .= shuffled


{-| Walk the list and then attempt to improve the SVM. Don't forget to shuffle the list! -}
solvePairs :: SVMParameters -> [TrainingSupportVector] -> ([TrainingSupportVector], Bool)
solvePairs params (x:xs) = let
  (tsv, _, success) = foldl (pairHelper params  (x:xs) ) ([], Just x, False) xs
  in
    (tsv, success)
solvePairs _ [] = ([], False)

{-| Helper function for solvePairs, it will run over the pairs and determine appropriate output,
 keeps a boolean to know if any of the pairs were successfully changed. -}
pairHelper :: SVMParameters
           -> [TrainingSupportVector]
           -> ([TrainingSupportVector], Maybe TrainingSupportVector, Bool)
           -> TrainingSupportVector
           -> ([TrainingSupportVector], Maybe TrainingSupportVector, Bool)
pairHelper _ _ (examined, Nothing, success) next =
  (examined, Just next, success)
pairHelper params allVecs (examined, Just target, success) next =
    case takeStepDetail params (WorkingState allVecs) target next  of
      Nothing -> ((next:examined), Just target, success)
      Just (r1, r2) -> (r1: r2:examined, Nothing, True)


-- Note to self, we could use the lens "zoom" to implement the reduced set heuristic
-- from the paper....
-- Also, the order of the training examples is arbitrary... I just want to choose two
-- and work with them, then choose a different two untill the list is exhausted...


{-| Construct a list of supportVector objects from the raw data  -}
{-| Note this initialised the weights all to 1. Could consider using a random number gen. -}
makeSupportVectors :: [RawFeatures] -> [ClassLabel] -> Maybe [TrainingSupportVector]
makeSupportVectors feat classes =
  let
    defTSV = constructTSV 1 (PredictClass1 1) 1 :: Sample ->  ClassLabel -> TrainingSupportVector
    inputs = zip (map (\x -> Sample x) feat) classes :: [(Sample, ClassLabel)]
    tsvList = map (\(f, c) -> defTSV f c) inputs :: [TrainingSupportVector]
  in
    if (length feat) == (length classes) then
      Just tsvList
    else
      Nothing


constructTSV :: Value
             -> PredictedLabel
             -> BaseScalar
             -> Sample
             -> ClassLabel
             -> TrainingSupportVector
constructTSV defValue pLabel alpha x label =
      TrainingSV {_trueLabel=label, _predLabel=pLabel,
                  _classError=defValue,
                  _supvec = SupportVector {_alpha=alpha, _vector=x}}


{-| HighLevel function to predict data given an SVM -}
predictSVM :: SVMParameters -> [RawFeatures] -> [PredictedLabel]
predictSVM params testData =
  let
    svs = params ^. supportVectors
    svm' = svm params svs
  in
    map svm' (map (\x -> x ^. vector) svs)


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
