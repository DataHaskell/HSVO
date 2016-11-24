{- SVM main implementation area -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Numeric.Algorithms.HSVO.SVM where


import Numeric.Algorithms.HSVO.Detail
import Control.Lens
import qualified Data.Vector as V

import Control.Monad.Reader --(MonadReader, Reader, ReaderT)
import Control.Monad.State --(MonadState, State, StateT)
import Control.Monad.Writer


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

Hmm... I guess I need to be careful with this design...

-}


takeStep :: SVMProblem ( TrainingSupportVector -> Maybe TrainingSupportVector )
takeStep =
  do
    vecs <- get
    params <- ask
    pure $ \x -> collateMaybes x [takeStepDetail params vec x  | vec <-  (_vectorList vecs ) ] -- list comprehension to evaluate over all maybes...

helper1 :: Maybe a -> [a] -> [a]
helper1 (Just a) b = a : b
helper1 Nothing b = b

maybeToLists :: [Maybe a] -> [a]
maybeToLists  =
  foldr (helper1) []




collateMaybes :: TrainingSupportVector -> [Maybe (TrainingSupportVector, TrainingSupportVector)] -> Maybe TrainingSupportVector
collateMaybes tsv1 tsvList =
  let
     validResults = WorkingState {_vectorList = snd <$> maybeToLists tsvList}
     alpha_data = foldrOf (vectorList . traverse . supvec . alpha) (\a (sum, count) -> ( (scalarToDbl a) + sum, count+1)) (0, 0) validResults
     sum =  fst alpha_data
     len =  snd alpha_data
     mean = sum / len
     tsv = firstOf (vectorList.traversed) validResults :: Maybe TrainingSupportVector
     makeNewTSV  = set (supvec . alpha) (wrapScalar mean) :: TrainingSupportVector -> TrainingSupportVector
     -- now extract the first element of validResults and modify it so that it has an alpha that is the mean value.
     -- now compute the new error for the global training example given that.

  in
    makeNewTSV <$> tsv
   -- We need to use the foldMaybe thing, where it returns two sets of lits

{-

I want a lens:
ASetter s s a b -> m a -> m b -> m ()
or maybe m (a -> b)

This would let me access the WorkingState out of the environment.

Ok, what about takeStep being, lets evaluate the desired alpha from all of the vectors.
Lets then average the contributions, sortof like stochastic gradient descent, except instead
of randomly doing it we will just eval all of them...



takeStep se this lets me examine a single element in the list and then modify it. However, I need to be
examining 2 elements from the lens and modifying both elements afterwards...

so we could try a sort of gibbs sampling style approach where we optimise only one, but calculate the required change
from all vectors... is this a case of constrained thinking generating creative outputs?

Hmmm... one problem is that we have a need to re-evaluate the SVM given its weights...


-}


{-

So we can use a list comprehension to express that we want to combine
the pair-wise associations of each, this will also work for a list of 2 lists that are not equal..

But, how to do this with lenses?

Lets not optimise this... instead lets write a lens that iterates over other lenses.
I.e. inside the traversal, lets scan through another lens traversal, but in this traversal, lets
check the svm parameter, and if its ok, go through and try and solve it.

It will not be particularly efficient, as we will need to visit every vector on every loop, even
though they are not needed. However, perhaps the compiler will help me...

-}



{-
   So, ok, we have this monad. It contains the current working state, as well as the parameters.
   We could use it all the way, i.e. if we are always working in this monad, then we can just access
   the stuff when we need to...
   How to make a stateT read only...
http://stackoverflow.com/questions/28587132/making-read-only-functions-for-a-state-in-haskell/28587399
   Consider using the subset pattern, to restrict the working state to only that which is neccesary...


-}




takeStepDetail :: SVMParameters
            -> TrainingSupportVector
            -> TrainingSupportVector
            -> Maybe (TrainingSupportVector, TrainingSupportVector)
takeStepDetail params sv1 sv2 = Just (sv1, sv2)
{-
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
-}

{-
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
    Just $ sn2 $ foldl findMax (0, target) boundList
-}
--- What I need is some structure that can abstract away the indexing into the vector.
-- Is this a monad?
-- I want to be able to perform operations over elements in the vector, with failure as an option, and
-- without having to pass around indecies... I guess I want a functor? (a-> Maybe b) -> [a] -> [b]
--
{-
shuffleList :: RandomGen g => ([a], g)
shuffleList l = error "implement shuffleList"
-}

{-
-- TODO: Will likely need to use a Monad Transformer to make a StateT MaybeT data structure...
examineExample :: RandomGen g => SVMParameters
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
-}
