{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Numeric.Algorithms.HSVO.Detail where
import Numeric.Algorithms.HSVO.Types

import Control.Lens

evaluateKernelWithWeight :: Kernel -> SupportVector -> Sample -> BaseScalar
evaluateKernelWithWeight k sv x = (sv ^. alpha) * (sv ^. vector) `k` x

svm :: SVMParameters -> [SupportVector] -> Sample -> PredictedLabel
svm params svl x =
             let
                k = params^.kernel
                b = params^.threshold
                res = (foldl (\a sv ->  (a + evaluateKernelWithWeight k sv x) )  0 svl ) - b
              in
                chooseClass res

-- |equation 15, 2nd Derivative
calcGrad :: Kernel -> Sample -> Sample -> BaseScalar
calcGrad k x1 x2 =  (x1 `k` x1) + (x2 `k` x2) - (2 * (x1 `k` x2) )

calcEta :: SVMParameters -> TrainingSupportVector -> TrainingSupportVector -> BaseScalar
calcEta params sv1 sv2 =
    calcGrad (params^.kernel) (sv1^.supvec.vector) (sv2^.supvec.vector)

classToDbl ::  ClassLabel -> Value
classToDbl Class1 = 1
classToDbl Class2 = -1

classToScalar :: ClassLabel -> BaseScalar
classToScalar a = wrapScalar $ classToDbl a


calcClassError :: ClassLabel -> PredictedLabel -> Value
calcClassError tLabel pLabel =
    let
        predVal = getLabelValue pLabel
        classVal = classToDbl tLabel
    in
        predVal - classVal


-- | equation 16, minimum along contstraint direction
alpha2New :: BaseScalar -- ^ Result of gradient calculation
             -> TrainingSupportVector -- ^ Training Point 1
             -> TrainingSupportVector -- ^ Training Point 2
             -> BaseScalar -- ^ New alpha2
alpha2New e sv1 sv2 = -- a e y1 y2 s1 s2 =
    let
        y2 = wrapScalar $ classToDbl $ sv2^.trueLabel
        e1 = sv1^.classError
        e2 = sv2^.classError
        diff = wrapScalar $ e1 - e2
        a = sv2^.supvec.alpha
    in
       a  + y2 * diff / e -- Implementing eq 16


-- | Equation 17,
alphaNewClipped :: Ord a => a -- ^ alpha2New
                   -> a                -- ^ H
                   -> a                -- ^ L
                   -> a
alphaNewClipped a h l
    | a >= h = h
    | a <= l = l
    | otherwise = a


scalarToValue :: BaseScalar -> Value
scalarToValue = id

calcS :: ClassLabel -> ClassLabel -> Value
calcS y1 y2 =
    classToDbl y1 * classToDbl y2


-- | Equation 18

alpha1New :: TrainingSupportVector     -- ^ Sample 1
            -> TrainingSupportVector  -- ^ Sample 2
            -> BaseScalar             -- ^ New alpha2
            -> BaseScalar             -- ^ New alpha2clipped
            -> BaseScalar
alpha1New sv1 sv2 a2 a2clip =
    let
        s = wrapScalar $ calcS (sv1^.trueLabel) (sv2^.trueLabel)
        a = sv1^.supvec.alpha
    in
        a + s * (a2 - a2clip)


f1 :: SVMParameters  -- ^ SVM Parameters
      -> TrainingSupportVector -- ^Vector1
      -> TrainingSupportVector -- ^vector2
      -> BaseScalar
f1 params sv1 sv2 =
    let
        k = params^.kernel
        b = params^.threshold
        a1 = sv1^.supvec.alpha
        a2 = sv2^.supvec.alpha
        y1 = sv1^.trueLabel
        y1' = wrapScalar $ classToDbl y1
        y2 = sv2^.trueLabel
        x1 = sv1^.supvec.vector
        x2 = sv2^.supvec.vector
        s   = wrapScalar $ calcS y1 y2
        k11 = x1 `k` x1
        k12 = x1 `k` x2
        e1 = wrapScalar $ calcClassError (sv1^.trueLabel) (sv1^.predLabel)
    in
        y1' * (e1 + b) -  a1 * k11 - s * a2 * k12


f2 :: SVMParameters --
      -> TrainingSupportVector -- ^ Vector1
      -> TrainingSupportVector -- ^ -- modify existing training vector
      -> BaseScalar
f2 params sv1 sv2 =
    let
        k = params^.kernel
        b = params^.threshold
        a1 = sv1^.supvec.alpha
        a2 = sv2^.supvec.alpha
        y1 = sv1^.trueLabel
        y2 = sv2^.trueLabel
        y2'= wrapScalar $ classToDbl y2
        x1 = sv1^.supvec.vector
        x2 = sv2^.supvec.vector
        s   = wrapScalar $ calcS y1 y2
        k22 = x2 `k` x2
        k12 = x1 `k` x2
        e2 = wrapScalar $ calcClassError (sv2^.trueLabel) (sv2^.predLabel)
    in
        y2' * (e2 + b) - s * a1 * k12 - a2 * k22


constructTrainingVec :: TrainingSupportVector
                        -> PredictedLabel
                        -> SupportVector
                        -> TrainingSupportVector
constructTrainingVec tsv label sv =
    let
        trueL = tsv^.trueLabel
        err = calcClassError trueL label
    in
        TrainingSV
            {
              _trueLabel = trueL
            , _predLabel = label
            , _supvec = sv
            , _classError = err
            }


-- equation 13
lowerAlpha :: SVMParameters
              -> TrainingSupportVector
              -> TrainingSupportVector
              -> Value
lowerAlpha params sv1 sv2 =
    let
        y1 = sv1^.trueLabel
        y2 = sv2^.trueLabel
        a1 = scalarToValue $ sv1^.supvec.alpha
        a2 = scalarToValue $ sv2^.supvec.alpha
        c = scalarToValue $ params^.margin
    in
        if not(y1 == y2)
        then
            max 0 (a1 - a2)
        else
            max 0 (a1 - a2 - c)


-- equation 14
upperAlpha :: SVMParameters
              -> TrainingSupportVector
              -> TrainingSupportVector
              -> Value
upperAlpha params sv1 sv2 =
    let
        y1 = sv1^.trueLabel
        y2 = sv2^.trueLabel
        a1 = scalarToValue $ sv1^.supvec.alpha
        a2 = scalarToValue $ sv2^.supvec.alpha
        c = scalarToValue $ params^.margin
    in
        if y1 /= y2
        then
            min c (c + a1 - a2)
        else
            min c (a1 - a2)


l1 :: SVMParameters
      -> TrainingSupportVector  -- sample1
      -> TrainingSupportVector -- sample2
      -> Value
l1 params sv1 sv2 =
    let
        y1 = sv1^.trueLabel
        y2 = sv2^.trueLabel
        a1 = scalarToValue $ sv1^.supvec.alpha
        a2 = scalarToValue $ sv2^.supvec.alpha
        c = scalarToValue $ params^.margin
        l = lowerAlpha params sv1 sv2
        s = calcS y1 y2
    in
        a1 + s * (a2 - l)

h1 :: SVMParameters
      -> TrainingSupportVector  -- sample1
      -> TrainingSupportVector -- sample2
      -> Value
h1 params sv1 sv2 =
    let
        y1 = sv1^.trueLabel
        y2 = sv2^.trueLabel
        a1 = scalarToValue $ sv1^.supvec.alpha
        a2 = scalarToValue $ sv2^.supvec.alpha
        c = scalarToValue $ params^.margin
        h = upperAlpha params sv1 sv2
        s = calcS y1 y2
    in
        a1 + s * (a2 - h)


psiLower :: SVMParameters
            -> TrainingSupportVector -- Sample1
            -> TrainingSupportVector -- Sample2
            -> BaseScalar
psiLower params sv1 sv2 = --a1 a2 y1 y2 x1 x2 t1 t2 l =
    let
        k = params^.kernel
        b = params^.threshold
        y1 = sv1^.trueLabel
        y2 = sv2^.trueLabel
        x1 = sv1^.supvec.vector
        x2 = sv2^.supvec.vector
        f1' = f1 params sv1 sv2
        f2' = f2 params sv1 sv2
        l1' = wrapScalar $ l1 params sv1 sv2
        l' = wrapScalar $ lowerAlpha params sv1 sv2
        s = wrapScalar $ calcS y1 y2
        k11 = x1 `k` x1
        k12 = x1 `k` x2
        k22 = x2 `k` x2
        half = wrapScalar 0.5
    in
       l1'* f1'
       + l'*f2'
       + half*l1'*l1'*k11
       + half*l1'*l1'*k22
       + s*l'*l1'*k12

psiUpper :: SVMParameters
            -> TrainingSupportVector -- Sample1
            -> TrainingSupportVector -- Sample2
            -> BaseScalar
psiUpper params sv1 sv2=
    let
        b = params^.threshold
        k = params^.kernel
        a1 = sv1^.supvec.alpha
        a2 = sv2^.supvec.alpha
        y1 = sv1^.trueLabel
        y2 = sv2^.trueLabel
        x1 = sv1^.supvec.vector
        x2 = sv2^.supvec.vector
        f1' = f1 params sv1 sv2
        f2' = f2 params sv1 sv2
        h1' = wrapScalar $ h1 params sv1 sv2
        h' = wrapScalar $ upperAlpha params sv1 sv2
        s = wrapScalar $ calcS y1 y2
        k11 = x1 `k` x1
        k12 = x1 `k` x2
        k22 = x2 `k` x2
        half = wrapScalar 0.5
    in
        h1'*f1' + h'*f2'
                + half * h1' * h1' * k11
                + half * h1' * h1' * k22
                + s * h' * h1' * k12


compareWithEps :: Value -> Value -> Value -> Bool
compareWithEps eps a b =
    if abs (a - b) <= eps then True else False


determineAtBound :: Value -> Value -> Value -> Bool
determineAtBound eps c a =
    let
        upper = compareWithEps eps c a
        zero = compareWithEps eps 0 a
    in
        case (upper, zero) of
            (False, False) -> False
            otherwise -> True


computeB :: SVMParameters
        -> Value -- alpha1new
        -> Value -- alpha2newclipped
        -> TrainingSupportVector -- support vector 1
        -> TrainingSupportVector -- support vector 2
        -> Value
computeB params a1new a2new sv1 sv2 =
    let
        eps = params^.epsillon
        c = scalarToValue $ params^.margin
        k = params^.kernel
        b = params^.threshold
        y1 = sv1^.trueLabel
        y2 = sv2^.trueLabel
        t1 = sv1^.predLabel
        t2 = sv2^.predLabel
        x1 = sv1^.supvec.vector
        x2 = sv2^.supvec.vector
        a1 = scalarToValue (sv1^.supvec.alpha)
        a2 = scalarToValue (sv2^.supvec.alpha)

        b' = scalarToValue b
        y1' = classToDbl y1
        y2' = classToDbl y2
        e1 = calcClassError y1 t1
        e2 = calcClassError y2 t2
        k11 = scalarToValue $ k x1 x1
        k12 = scalarToValue $ k x1 x2
        k22 = scalarToValue $ k x2 x2

        b1 = e1 + y1'*(a1new - a1)*k11
                + y2'*(a2new - a2)*k12 + b'

        b2 = e2 + y1'*(a1new - a1)*k12
                + y2'*(a2new-a2)*k22 + b'
        a1atBound = determineAtBound eps c a1new
        a2atBound = determineAtBound eps c a2new

    in
        case (a1atBound, a2atBound) of
            (True, True) -> (b1+b2)/2
            (False, True) -> b2
            (True, False) -> b1
            (False, False) -> b1 -- (Note b1 should equal b2 in this instance?)



elementDifference :: Sample -> Sample -> BaseVector
elementDifference (Sample v1) (Sample v2) =
    (v1 -^ v2)


sumVector :: BaseVector -> Value
sumVector v =
    foldl (+) 0 v

etaOutOfBounds :: SVMParameters
                    -> TrainingSupportVector
                    -> TrainingSupportVector
                    -> Value
etaOutOfBounds params sv1 sv2 =
    let
       eps = params^.epsillon
       alpha2 = scalarToValue $ sv2^.supvec.alpha
       h_obj = scalarToValue $ psiUpper params sv1 sv2
       l_obj = scalarToValue $ psiLower params sv1 sv2
       l = l1 params sv1 sv2
       h = h1 params sv1 sv2
    in
        if l_obj < h_obj - eps then l
        else
            if l_obj > h_obj + eps then h
            else alpha2


-- | This function determines what alpha2 should be but can fail.
determineAlpha2 :: SVMParameters
                   -> TrainingSupportVector
                   -> TrainingSupportVector
                   -> Maybe (Value, Value)
determineAlpha2 params sv1 sv2 =
    let
        eps = params^.epsillon
        eta = calcEta params sv1 sv2
        l =  l1 params sv1 sv2
        h =  h1 params sv1 sv2
        alpha1 = scalarToValue $ sv1^.supvec.alpha
        alpha2 = scalarToValue $ sv2^.supvec.alpha
        a2clip = alphaNewClipped a2' h l
        outOfBounds = etaOutOfBounds params sv1 sv2
        a2' = scalarToValue $ alpha2New eta sv1 sv2
        a2 = if scalarToValue (eta) > 0 then a2clip else outOfBounds
    in
     do
       _ <- if (abs(a2-alpha2) < eps*(a2+alpha2+eps)) then Nothing else Just ()
       Just (a2, a2clip)
