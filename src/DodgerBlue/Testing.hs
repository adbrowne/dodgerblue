{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module DodgerBlue.Testing
  (evalDslTest,
   evalMultiDslTest,
   HasDslEvalState(..),
   MemQ.Queue,
   MemQ.Queues,
   emptyEvalState)
where

import           Control.Lens
import           Data.Maybe (fromJust)
import           Control.Monad.Free.Church
import qualified Control.Monad.Free                     as Free
import           Control.Monad.State
import           Data.Text (Text)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Typeable
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import qualified DodgerBlue.InMemoryQueues as MemQ
import           DodgerBlue.Types

newtype ThreadTree = ThreadTree { unThreadTree :: Int }
newtype Node = Node { unNode :: Text }

data ProgramPosition t a =
  External (Free.Free (CustomDsl MemQ.Queue t) a)
  | Internal (Free.Free (CustomDsl MemQ.Queue t) ())

data EvalThread t a = EvalThread {
  evalThreadName :: Text,
  evalThreadTree :: ThreadTree,
  evalThreadNode :: Node,
  evalThreadPosition :: ProgramPosition t a }

data EvalState = EvalState {
  _evalStateQueues :: MemQ.Queues
}

emptyEvalState :: EvalState
emptyEvalState = EvalState {
  _evalStateQueues = MemQ.emptyQueues }

$(makeLenses ''EvalState)

testQueues :: HasDslEvalState a => Lens' a MemQ.Queues
testQueues = evalStateLens . evalStateQueues

runWriteQueueCmd :: (MonadState s m, HasDslEvalState s, Typeable a) => MemQ.Queue a -> a -> m ()
runWriteQueueCmd q x = do
  testQueues %= (\qs -> MemQ.writeToQueue qs q x)

runTryReadQueueCmd :: (MonadState s m, HasDslEvalState s, Typeable a) => MemQ.Queue a -> m (Maybe a)
runTryReadQueueCmd q = do
  qs <- use testQueues
  let (x, qs') = MemQ.tryReadFromQueue qs q
  testQueues .= qs'
  return x

runNewQueueCmd :: (MonadState s m, HasDslEvalState s, Typeable a) => m (MemQ.Queue a)
runNewQueueCmd = do
  qs <- use testQueues
  let (x, qs') = MemQ.newQueue qs
  testQueues .= qs'
  return x

class HasDslEvalState a where
  evalStateLens :: Lens' a EvalState

instance HasDslEvalState EvalState where
  evalStateLens = id

type TestCustomCommandStep t m = forall a. t (Free.Free (CustomDsl MemQ.Queue t) a) -> m (Free.Free (CustomDsl MemQ.Queue t) a)

data ThreadGroup t a = ThreadGroup { threadGroupPrograms :: [(Text, F (CustomDsl MemQ.Queue t) a)] }

data ThreadResultGroup a = ThreadResultGroup {
  threadResultGroupPrograms :: [(Text, a)] }

data LoopState t a = LoopState {
  loopStatePrograms :: Seq (EvalThread t a),
  loopStateResults :: Map Text (ThreadResultGroup a) }

data StepResult t a =
  StepResultComplete a
  | StepResultContinue (Free.Free (CustomDsl MemQ.Queue t) a)
  | StepResultFork ((Free.Free (CustomDsl MemQ.Queue t) a)) (Free.Free (CustomDsl MemQ.Queue t) ())

stepProgram :: (MonadState s m, HasDslEvalState s, Monad m, Functor t) => TestCustomCommandStep t m -> Free.Free (CustomDsl MemQ.Queue t) a -> m (StepResult t a)
stepProgram _ (Free.Pure a) = return . StepResultComplete $ a
stepProgram _ (Free.Free (DslBase (NewQueue' n))) = do
  q <- runNewQueueCmd
  return $ StepResultContinue (n q)
stepProgram _ (Free.Free (DslBase (WriteQueue' q a n))) =
  runWriteQueueCmd q a >> return (StepResultContinue n)
stepProgram _ (Free.Free (DslBase (TryReadQueue' q n))) =
  runTryReadQueueCmd q >>= return . StepResultContinue . n
stepProgram _ cmd@(Free.Free (DslBase (ReadQueue' q n))) = do
  result <- runTryReadQueueCmd q
  case result of Just a  -> return $ StepResultContinue (n a)
                 Nothing -> return (StepResultContinue cmd)
stepProgram _ (Free.Free (DslBase (ForkChild' childProgram n))) =
  return (StepResultFork n (fromF childProgram))
stepProgram _ (Free.Free (DslBase _a)) = error $ "todo DslBase _"
stepProgram stepCustomCommand (Free.Free (DslCustom cmd)) =
  stepCustomCommand cmd >>= return . StepResultContinue

buildLoopState :: Functor t => Map Text (ThreadGroup t a) -> LoopState t a
buildLoopState threadMap =
  let
    threadQueue = fst $ Map.foldlWithKey buildThreadQueue (Seq.empty,0) threadMap
    buildThreadQueue (tq, i) threadGroupName ThreadGroup{..} =
      let
        node = Node { unNode = threadGroupName }
        threadTree = ThreadTree { unThreadTree = i }
        newQueueItems = Seq.fromList $ buildEvalThread node threadTree <$> threadGroupPrograms
        tq' = newQueueItems Seq.>< tq
      in (tq',i + 1)
    buildEvalThread node threadTree (name, startPosition) = EvalThread {
      evalThreadName = name,
      evalThreadPosition = External (fromF startPosition),
      evalThreadNode = node,
      evalThreadTree = threadTree }
  in
    LoopState {
      loopStatePrograms = threadQueue,
      loopStateResults = mempty }

stepEvalThread ::
  (MonadState s m, HasDslEvalState s, Functor t) =>
  TestCustomCommandStep t m ->
  EvalThread t a ->
  m (LoopState t a -> LoopState t a)
stepEvalThread stepCustomCommand evalThread@EvalThread{ evalThreadPosition = Internal p } = do
  result <- stepProgram stepCustomCommand p
  case result of StepResultComplete () -> return $ id -- don't worry about complete internal threads
                 StepResultContinue n -> return $ updateThread n
                 StepResultFork n newProgram -> return $ (addThread newProgram) . (updateThread n)
  where
    addThread childProgram ls =
      let newThread = evalThread { evalThreadPosition = Internal childProgram }
      in ls { loopStatePrograms = newThread Seq.<| (loopStatePrograms ls) }
    updateThread n ls =
      let evalThread' = evalThread { evalThreadPosition = Internal n }
      in ls { loopStatePrograms = evalThread' Seq.<| (loopStatePrograms ls) }
stepEvalThread stepCustomCommand evalThread@EvalThread { evalThreadPosition = External p } = do
  result <- stepProgram stepCustomCommand p
  case result of StepResultComplete a -> return $ addResult evalThread a
                 StepResultContinue n -> return $ updateThread n
                 StepResultFork n newProgram -> return $ (addThread newProgram) . (updateThread n)
  where
    addResult EvalThread{..} a ls@LoopState{..}=
      let
        resultGroup = ThreadResultGroup [(evalThreadName, a)]
        loopStateResults' = Map.insert (unNode evalThreadNode) resultGroup loopStateResults
      in ls { loopStateResults = loopStateResults' }
    addThread childProgram ls =
      let newThread = evalThread { evalThreadPosition = Internal childProgram }
      in ls { loopStatePrograms = newThread Seq.<| (loopStatePrograms ls) }
    updateThread n ls =
      let evalThread' = evalThread { evalThreadPosition = External n }
      in ls { loopStatePrograms = evalThread' Seq.<| (loopStatePrograms ls) }

evalMultiDslTest ::
  (MonadState s m, HasDslEvalState s, Functor t) =>
  TestCustomCommandStep t m ->
  Map Text (ThreadGroup t a) ->
  m (Map Text (ThreadResultGroup a))
evalMultiDslTest stepCustomCommand threadMap =
  let
    loopState = buildLoopState threadMap
  in go loopState
  where
    go loopState@LoopState { .. } =
      case Seq.viewr loopStatePrograms of Seq.EmptyR      -> return loopStateResults
                                          (ps Seq.:> p)   -> progressThread loopState p ps
    progressThread loopState p ps = do
      f <- stepEvalThread stepCustomCommand p
      let loopState' = f $ loopState { loopStatePrograms = ps }
      go loopState'

evalDslTest ::
  (MonadState s m, HasDslEvalState s, Functor t) =>
  TestCustomCommandStep t m ->
  F (CustomDsl MemQ.Queue t) a ->
  m a
evalDslTest stepCustomCommand p =
  let
    mainThreadKey = "MainThread"
    inputMap = Map.singleton mainThreadKey (ThreadGroup { threadGroupPrograms = [(mainThreadKey, p)]})
    resultSet = evalMultiDslTest stepCustomCommand inputMap
  in
    fromJust . (lookup mainThreadKey) . threadResultGroupPrograms . (Map.! mainThreadKey) <$> resultSet
