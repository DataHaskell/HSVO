{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Numeric.Algorithms.HSVO.Types where

import Control.Lens
import Generics.Deriving

{-

-}

(*^) :: Num a => [a] -> [a] -> [a]
(*^) = zipWith (*)

(-^) :: Num a => [a] -> [a] -> [a]
(-^) = zipWith (-)


type Value = Double

type BaseVector = [Value]
type BaseScalar = Value

-- Making these types to attempt to make the system more type-safe
newtype Sample = Sample BaseVector deriving (Show)
newtype Weights = Weights BaseVector deriving (Show)

weightAsSample :: Weights -> Sample
weightAsSample (Weights w) = Sample w

type Threshold = BaseScalar

-- There is probably an opportunity to build a type-class around the
-- rules for composing kernel functions.
type Kernel = Sample -> Sample -> BaseScalar

data ClassLabel = Class1 | Class2 deriving (Show, Eq)

-- An SVM prediction also contains the "margin" that can be useful for solving
data PredictedLabel = PredictClass1 Value | PredictClass2 Value deriving (Show, Eq)


data SupportVector = SupportVector {
                         _alpha :: BaseScalar
                       , _vector :: Sample
                       } deriving (Show, Generic)

data TrainingSupportVector = TrainingSV {
                             _trueLabel :: ClassLabel
                           , _predLabel :: PredictedLabel
                           , _classError :: Value
                           , _supvec :: SupportVector
                        } deriving (Show, Generic)

data SVMParameters = SVMParameters {
                        _kernel :: Kernel
                     ,  _maxIters :: Int
                     ,  _threshold :: Threshold
                     ,  _margin :: BaseScalar     -- ^ parameter C in eq. 9
                     ,  _epsillon :: Value        -- ^ rounding error for equality
                     ,  _supportVectors :: [SupportVector]
                    } deriving (Generic)

data TrainingData = TrainingData {_training :: [TrainingSupportVector]}
                     deriving ( Generic)

makeLenses ''SupportVector
makeLenses ''TrainingSupportVector
makeLenses ''SVMParameters
makeLenses ''TrainingData

getVec a = a^.(supvec . vector)
getAlpha a = a^. (supvec . alpha)


-- PredictedLabel helpers

getLabelValue :: PredictedLabel -> Value
getLabelValue (PredictClass1 v) = v
getLabelValue (PredictClass2 v) = v

predictToTrue :: PredictedLabel -> ClassLabel
predictToTrue (PredictClass1 _) = Class1
predictToTrue (PredictClass2 _) = Class2

wrapScalar :: Value -> BaseScalar
wrapScalar = id

-- Building an SVM

chooseClass :: Value -> PredictedLabel
chooseClass res = if res >= 0 then PredictClass1 res else PredictClass2 res

dot :: Kernel
dot (Sample a) (Sample b) = foldl (+) 0 ( a *^ b)


--import Control.Foldl

-- We need to be able to pass both the global parameters, but also some state parameters
-- in the form of the vectors ....



-- | The main list of things...
newtype WorkingState = WorkingState
                     {
                       _vectorList :: [TrainingSupportVector]
                     }

makeLenses ''WorkingState


{-newtype SVMProblem a = SVMProblem {
                         runProb :: StateT Int (WriterT String (Reader Int) ) a
                         }
                       deriving (MonadState Int , MonadWriter String, MonadReader Int)
-}

focusOnSupportVector :: WorkingState -> [SupportVector]
focusOnSupportVector ws = ws ^.. vectorList . traversed . supvec
