module Main where

import Data.Vector as V
import Data.Array.Repa
import Data.Array.Repa.Repr.Vector
import Data.Array.Repa.Algorithms.Matrix
import Numeric.Algorithms.HSVO.SVM
import Numeric.Algorithms.HSVO.Detail

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader

bVec = V.fromList [0, 1 , 3 ] :: V.Vector Integer

s1 =  [0.1, 0.2]

listToSample ::  [Value] -> Sample
listToSample a = Sample $ fromListUnboxed (Z :. Prelude.length a) a

scalarToRepa :: Value -> BaseScalar
scalarToRepa a = fromListUnboxed (Z) [a]

sv1 = SupportVector {
                      _alpha= scalarToRepa 0.3
                    , _vector = listToSample s1
                    }

tsvO = TrainingSV {
                   _trueLabel = Class1
                 , _predLabel = PredictClass2 0.2
                 , _classError = calcClassError Class1 $ PredictClass2 0.2
                 , _supvec = sv1
                 }

stupidKernel :: Kernel
stupidKernel _ _ = scalarToRepa 0.1

params = SVMParameters {
                         _kernel = stupidKernel
                       , _threshold = scalarToRepa 0.1
                       , _margin = scalarToRepa 0.1
                       , _epsillon = 0.3
                       }

tData = TrainingData {_training = V.fromList [tsvO]}


{-
  So I want to see what happens if I have various things in the StateT monad.
 -}

temp :: StateT WorkingState IO ()
temp =
  do
    a <- get
    pure ()

main :: IO ()
main = putStrLn $ show bVec
