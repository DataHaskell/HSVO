module Numeric.Algorithms.HSVO.SVM where
{-# LANGUAGE TemplateHaskell #-}
import Numeric.Algorithms.HSVO.Detail

data TargetVector = TargetVector
               {
                 _tsv :: TrainingSupportVector,
                 _index :: Int
               }
''makeLens TargetVector               


takeStep :: SVMParameters
            -> TrainingSupportVector
            -> TrainingSupportVector
            -> Maybe (TrainingSupportVector, TrainingSupportVector)
takeStep params sv1 sv2 =
    let
        x1 = sv1^.supvec.vector
        x2 = sv2^.supvec.vector
        diff = sumVector (elementDifference x1 x2)
        identical = abs (diff) < params^.epsillon
        sVectors = V.map (\a-> a^.supvec) tData
    in
        do
            -- First step, check that the vectors are identical.
            _ <- pure (if identical then Nothing else Just ())
            (a2, a2clip) <- determineAlpha2 params sv1 sv2
            a1 <- pure $ alpha1New sv1 sv2 (wrapScalar a2) (wrapScalar a2clip)
            sv1' <- pure $ SupportVector {_alpha= a1, _vector=sv1^.supvec.vector}
            sv2' <- pure $ SupportVector {_alpha=wrapScalar (a2), _vector=sv2^.supvec.vector}
            newSvec <- pure $ V.toList $ sVectors V.// [(i, sv1'), (j, sv2')]
            pred1 <- pure $ svm params newSvec x1
            pred2 <- pure $ svm params newSvec x2
            finalSv1 <- pure $ constructTrainingVec sv1 pred1 sv1'
            finalSv2 <- pure $ constructTrainingVec sv2 pred2 sv2'
            -- Next evaluate SVM using the new results for a1 and a2
            -- Looks like I will need a complete SVM copy, and to make a
            -- new training set... can I build a traverse?
            return (finalSv1, finalSv2)-- modify existing training vector


trainToSv :: V.Vector TrainingSupportVector -> V.Vector Sample
trainToSv = V.map (\a -> a^.supvec.vector )


-- | Second choice heuristic will choose to maximise the expected change in error
--   from optimising this step. See paper for more details.
secondChoiceHeuristic :: [TargetVector] -> TargetVector -> Maybe TargetVector
secondChoiceHeuristic [] _ = Nothing 
secondChoiceHeuristic boundList target =
  let
    targetErr = target ^.tsv. classError
    findMax (best, tv1) tv2 = if current > best then (current, tv2) else (best, tv1)
                                        where current = abs (tv1 ^. classError - tv2 ^. classError)
                                              
  in
    Just $ sn2 $ foldl findMax (0, ) boundList 

indiciesAndVecToList :: [Int] -> V.Vector TrainingSupportVector -> [TrainingSupportVector]
indiciesAndVecToList iList tData = map (\x -> tData V.! i ) tData

--- What I need is some structure that can abstract away the indexing into the vector.
-- Is this a monad?
-- I want to be able to perform operations over elements in the vector, with failure as an option, and
-- without having to pass around indecies... I guess I want a functor? (a-> Maybe b) -> [a] -> [b]
--

shuffleList :: RandomGen g => ([a], g)
shuffleList l = error "implement shuffleList"



-- TODO: Will likely need to use a Monad Transformer to make a StateT MaybeT data structure...
examineExample :: RandomGen g => SVMParametersp
               -> [TargetVector]                  -- ^ Complete list of all vectors.
               -> [TargetVector]                    -- ^ List of all non-zerod or non-maxed indexes into trainData
               -> TargetVector                        -- ^ Selected target i                   
               -> g   -- random number Generator
               -> Maybe (TargetVector, TargetVector)
examineExample params allVec boundList target g =
  let
    sv = target ^. tsv
    y2 = sv ^. trueLabel
    alph2 = sv ^. supvec ^. alpha
    t2 = sv ^. predLabel
    e2 = calcClassError y2 t2
    r2 = e2 * (classToDbl y2)
    margin = params ^. margin
    scdHeur = secondChoiceHeuristic tData boundList i
    stepFunc = takeStep params sv   -- partial application, have a single parameter left. 
    (shuffledBoundList, g1) = shuffleList boundList
    (allVectorList, g2) = shuffleList allVec
    
    boundListExamined = map stepFunc shuffledBoundList
    allVectorsExamined = map stepFunc allVectorsList
    examineList = msm $ scdHeur : (boundListExamined ++ allVectorList)
  
    
  in do
    _ <- if (r2 < -tol && alph2 < c) || (r2 > tol && alph2 > 0) then Just () else Nothing
    take 1 examineList -- ^ Make sure to use a version of take that can handle empty lists...

    -- See if we can do a random shuffle of the indecies, then we can just map over a list or zip to create the first [Maybe a]
    -- qn, do we shuffle first over the boundList ? I think so... then we shuffle over everything else. 

    -- So to solve this, create a list of maybe results, one for each of the cases.
    -- Then we concat all the lits of maybes together and use msum from MonadPlus.
    -- This will return just the first non-nothing value, or if none was found, it
    -- itself will return Nothing. Hopefully due to lazy evaluation it will only
    -- actually eval up untill the first Non-Nothing in the list!
