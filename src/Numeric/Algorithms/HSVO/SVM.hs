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


takeStepBlah :: SVMProblem ()
takeStepBlah =
  do
    vecs <- get :: SVMProblem WorkingState
    params <- ask :: SVMProblem SVMParameters
    -- <. combines an indexed lens.
    -- %@= , accesses an indexed lens inside the state monad over a traversal...
    -- simple case is just re-assigning stuff... should be able to use it to make
    -- more useful things.
--    ((vectorList . itraversed <. tsv) <. classError) %@= (\i _ -> (vecs ^. vectorList . traversed . tsv)  !! i)
    return ()


{-
It appears that I might need to reconsider the design again...
I want some state inside the takestep loop so I can know what the change is... well, I guess I can try and pull that out....
-}

takeStepForEachOtherTSV :: SVMParameters -> WorkingState -> TrainingSupportVector -> Maybe TrainingSupportVector
takeStepForEachOtherTSV params vecs x = collateMaybes x [takeStepDetail params vecs v x | v <- (_vectorList vecs) ]


takeStep :: SVMProblem ( )
takeStep =
  do
    vecs <- get
    params <- ask
    -- Use a prism to apply the result of collateMaybes every TSV.

    pure ()

helper1 :: Maybe a -> [a] -> [a]
helper1 (Just a) b = a : b
helper1 Nothing b = b


collateMaybes :: TrainingSupportVector -> [Maybe (TrainingSupportVector, TrainingSupportVector)] -> Maybe TrainingSupportVector
collateMaybes tsv1 tsvList =
  let
     foldFunc a (sum, count) = ( (scalarToValue a) + sum, count + 1 )
     validResults = WorkingState {_vectorList = snd <$> catMaybes tsvList}
     alpha_data = foldrOf (vectorList . traverse . supvec . alpha) foldFunc (0, 0) validResults
     (sum, len) = alpha_data
     mean = sum / len
     tsv = firstOf (vectorList.traversed) validResults :: Maybe TrainingSupportVector
     makeNewTSV  = set (supvec . alpha) (wrapScalar mean) :: TrainingSupportVector -> TrainingSupportVector
     -- now extract the first element of validResults and modify it so that it has an alpha that is the mean value.
     -- now compute the new error for the global training example given that.

  in
    makeNewTSV <$> tsv


extractSupportVectors :: WorkingState -> [SupportVector]
extractSupportVectors ws = ws ^.. vectorList . traversed . supvec


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
