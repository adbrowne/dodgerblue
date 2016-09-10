{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DodgerBlue.Testing
  (evalDslTest,
   evalMultiDslTest,
   customCmd,
   MemQ.Queue,
   MemQ.Queues,
   ThreadGroup(..),
   ThreadResult(..),
   TestProgram,
   TestEvaluator(..),
   EvalState,
   ExecutionTree(..),
   ThreadResultGroup(..),
   loopStateLastRan,
   emptyEvalState)
where

import           Control.Lens
import           Debug.Trace
import           Data.Maybe (fromJust, catMaybes, fromMaybe)
import           Data.Monoid
import           Data.List.NonEmpty hiding (head, dropWhile, filter)
import           Control.Monad.Free.Church
import qualified Control.Monad.Free                     as Free
import           Control.Monad.State
import           Control.Monad.Reader
import qualified Control.Monad.Trans.State.Lazy as LazyState
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Typeable
import qualified DodgerBlue.InMemoryQueues as MemQ
import           DodgerBlue.Types
import           Safe
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

type TestProgram t a = F (CustomDsl MemQ.Queue t) a
type TestProgramFree t a = Free.Free (CustomDsl MemQ.Queue t) a

data ProgramState t a =
  ExternalProgramRunning (TestProgramFree t a, Maybe Int)
  | ExternalProgramComplete a
  | InternalProgramRunning (TestProgramFree t (), Maybe Int)

mkExternalProgramRunning :: TestProgramFree t a -> ProgramState t a
mkExternalProgramRunning p = ExternalProgramRunning (p, Nothing)

mkInternalProgramRunning :: TestProgramFree t () -> ProgramState t a
mkInternalProgramRunning p = InternalProgramRunning (p, Nothing)

data EvalState = EvalState {
  _evalStateQueues :: MemQ.Queues
}

newtype ExecutionTree a = ExecutionTree { unExecutionTree :: Map Text a }
  deriving (Eq,Show)

instance Arbitrary a => Arbitrary (ExecutionTree a) where
  arbitrary = ExecutionTree <$> arbitrary

instance Foldable ExecutionTree where
  foldMap f (ExecutionTree t) = foldMap f t

instance Traversable ExecutionTree where
  traverse f (ExecutionTree t) = ExecutionTree <$> traverse f t

instance Functor ExecutionTree where
  fmap f (ExecutionTree t) = ExecutionTree $ fmap f t

mapMaybeExecutionTree :: (a -> Maybe b) -> ExecutionTree a -> ExecutionTree b
mapMaybeExecutionTree f ExecutionTree{..} =
  ExecutionTree $ Map.mapMaybe f unExecutionTree

updateWithKeyExecutionTree :: (a -> Maybe a) -> Text -> ExecutionTree a -> ExecutionTree a
updateWithKeyExecutionTree f threadName (ExecutionTree t) =
  ExecutionTree $ (Map.update f threadName) t

getExecutionTreeEntry :: Text -> ExecutionTree a -> Maybe a
getExecutionTreeEntry threadName ExecutionTree{..} =
  Map.lookup threadName unExecutionTree

emptyEvalState :: EvalState
emptyEvalState = EvalState {
  _evalStateQueues = MemQ.emptyQueues }

$(makeLenses ''EvalState)

data LoopState t r = LoopState {
  _loopStatePrograms :: ExecutionTree (ProgramState t r),
  _loopStateTestState :: EvalState,
  _loopStateIterations :: Int,
  _loopStateLastRan :: Maybe (Text),
  _loopStateInIdleCoolDown :: Bool }

$(makeLenses ''LoopState)

testQueues :: Lens' (LoopState t a) MemQ.Queues
testQueues = loopStateTestState . evalStateQueues

runWriteQueueCmd :: (MonadState (LoopState t r) m, Typeable a) => MemQ.Queue a -> a -> m ()
runWriteQueueCmd q x = do
  testQueues %= (\qs -> MemQ.writeToQueue qs q x)

runTryReadQueueCmd :: (MonadState (LoopState t r) m, Typeable a) => MemQ.Queue a -> m (Maybe a)
runTryReadQueueCmd q = do
  qs <- use testQueues
  let (x, qs') = MemQ.tryReadFromQueue qs q
  testQueues .= qs'
  return x

runNewQueueCmd :: (MonadState (LoopState t r) m, Typeable a) => m (MemQ.Queue a)
runNewQueueCmd = do
  qs <- use testQueues
  let (x, qs') = MemQ.newQueue qs
  testQueues .= qs'
  return x

type TestCustomCommandStep t m = forall a. t a -> m a

data ThreadGroup t a = ThreadGroup { threadGroupPrograms :: Map Text (TestProgram t a) }

instance Show (ThreadGroup t a) where
  show a = "ThreadGroup { threadGroupPrograms = " <> show ((Map.keys . threadGroupPrograms) a) <> " }"

data ThreadResult a =
  ThreadResult a
  | ThreadBlocked
  | ThreadIdle
  deriving (Eq, Show)

fromThreadResult :: ThreadResult a -> a
fromThreadResult (ThreadResult a) = a
fromThreadResult ThreadBlocked = error "fromThreadResult: thread blocked"
fromThreadResult ThreadIdle = error "fromThreadResult: thread idle"

data ThreadResultGroup a = ThreadResultGroup {
  threadResultGroupPrograms :: Map Text (ThreadResult a) }
  deriving (Eq,Show)

data StepResult t a =
  StepResultComplete a
  | StepResultContinue (TestProgramFree t a, Maybe Bool)
  | StepResultFork (TestProgramFree t a) Text (TestProgramFree t ())

standardContinueStep :: TestProgramFree t a -> StepResult t a
standardContinueStep n = StepResultContinue (n, Nothing)

stepProgram :: (MonadState (LoopState t r) m, Functor t) => TestCustomCommandStep t m -> TestProgramFree t a -> m (StepResult t a)
stepProgram _ (Free.Pure a) = return . StepResultComplete $ a
stepProgram _ (Free.Free (DslBase (NewQueue' n))) = do
  q <- runNewQueueCmd
  return $ standardContinueStep (n q)
stepProgram _ (Free.Free (DslBase (WriteQueue' q a n))) =
  runWriteQueueCmd q a >> return (standardContinueStep n)
stepProgram _ (Free.Free (DslBase (TryReadQueue' q n))) =
  runTryReadQueueCmd q >>= return . standardContinueStep . n
stepProgram _ cmd@(Free.Free (DslBase (ReadQueue' q n))) = do
  result <- runTryReadQueueCmd q
  case result of Just a  -> return $ standardContinueStep (n a)
                 Nothing -> return (standardContinueStep cmd)
stepProgram _ (Free.Free (DslBase (ForkChild' childName childProgram n))) =
  return (StepResultFork n childName (fromF childProgram))
stepProgram _ (Free.Free (DslBase (SetPulseStatus' active n))) =
  return (StepResultContinue (n, Just active))
stepProgram stepCustomCommand (Free.Free (DslCustom cmd)) =
  stepCustomCommand cmd >>= return . standardContinueStep

buildLoopState :: Functor t => ExecutionTree (TestProgram t r) -> EvalState -> LoopState t r
buildLoopState threadMap testState =
    LoopState {
      _loopStatePrograms = fmap (mkExternalProgramRunning . fromF) threadMap,
      _loopStateTestState = testState,
      _loopStateIterations = 0,
      _loopStateLastRan = mempty,
      _loopStateInIdleCoolDown = False }

updateIdleCount :: Maybe Bool -> Maybe Int -> Maybe Int
updateIdleCount Nothing x = x
updateIdleCount (Just True) _ = Just 0
updateIdleCount (Just False) Nothing = Just 1
updateIdleCount (Just False) (Just x) = Just $ x + 1

stepEvalThread ::
  (MonadState (LoopState t r) m, Functor t) =>
  TestCustomCommandStep t m ->
  ProgramState t r ->
  m (Maybe (ProgramState t r), Maybe (Text, ProgramState t r))
stepEvalThread stepCustomCommand (InternalProgramRunning (p,idleCount)) = do
  result <- stepProgram stepCustomCommand p
  case result of StepResultComplete () -> return $ (Nothing, Nothing)
                 StepResultContinue (n,isActive) -> return $ (Just (InternalProgramRunning (n, updateIdleCount isActive idleCount)), Nothing)
                 StepResultFork n childName newProgram -> return $ (Just (InternalProgramRunning (n, idleCount)), Just  (childName, mkInternalProgramRunning newProgram))
stepEvalThread stepCustomCommand (ExternalProgramRunning (p,idleCount)) = do
  result <- stepProgram stepCustomCommand p
  case result of StepResultComplete a -> return $ (Just (ExternalProgramComplete a), Nothing)
                 StepResultContinue (n,isActive) -> return $ (Just (ExternalProgramRunning (n, updateIdleCount isActive idleCount)), Nothing)
                 StepResultFork n childName newProgram -> return $ (Just (ExternalProgramRunning (n,idleCount)), Just (childName, mkInternalProgramRunning newProgram))
stepEvalThread _ (ExternalProgramComplete a) = return (Just (ExternalProgramComplete a), Nothing)

buildResults :: MonadState (LoopState t a) m => m (ExecutionTree (ThreadResult a))
buildResults  = do
  LoopState {..} <- get
  return $ mapMaybeExecutionTree toResult _loopStatePrograms
  where
    toResult (InternalProgramRunning _) = Nothing
    toResult (ExternalProgramComplete a) = Just $ ThreadResult a
    toResult (ps@(ExternalProgramRunning _)) =
      if isProgramStateIdle ps then
        Just ThreadIdle
      else
        Just ThreadBlocked

foldlWithKey' :: (s -> Text -> a -> s) -> s -> ExecutionTree a -> s
foldlWithKey' acc z (ExecutionTree t) = Map.foldlWithKey' (\s threadName v -> acc s threadName v) z t

isProgramBlocked :: (MonadState (LoopState t a) m, Functor t) => TestProgramFree t b -> m (Bool)
isProgramBlocked (Free.Free (DslBase (ReadQueue' q _n))) = do
  qs <- use testQueues
  return $ MemQ.isEmptyQueue qs q
isProgramBlocked _ = return False

allIncompletePrograms :: (Functor t) => ExecutionTree (ProgramState t a) -> [(Text,ProgramState t a)]
allIncompletePrograms t = catMaybes $ foldlWithKey' acc [] t
 where acc xs threadName programState =
         (justIfRunnable threadName programState):xs
       justIfRunnable threadName programState@(ExternalProgramRunning _) = 
         Just (threadName, programState)
       justIfRunnable _ (ExternalProgramComplete _) =
         Nothing
       justIfRunnable threadName programState@(InternalProgramRunning _) = 
         Just (threadName, programState)

runnablePrograms :: (MonadState (LoopState t a) m, Monad m, Functor t) => ExecutionTree (ProgramState t a) -> m [(Text,ProgramState t a)]
runnablePrograms  t = (fmap catMaybes) . sequenceA $ foldlWithKey' acc [] t
 where acc xs threadName programState =
         (justIfRunnable threadName programState):xs
       justIfRunnable threadName programState@(ExternalProgramRunning (p,_idleCount)) = do
         isBlocked <- isProgramBlocked p
         if isBlocked then
           return Nothing
         else return $ Just (threadName, programState)
       justIfRunnable _ (ExternalProgramComplete _) =
         return Nothing
       justIfRunnable threadName programState@(InternalProgramRunning (p,_idleCount)) = do
         isBlocked <- isProgramBlocked p
         if isBlocked then
           return $ Nothing
         else return $ Just (threadName, programState)

class TestEvaluator m where
  chooseNextThread :: Maybe Text -> NonEmpty (Text, a) -> m (Text, a)

instance TestEvaluator Identity where
  chooseNextThread Nothing ((k,x) :| _) = return (k,x)
  chooseNextThread (Just lastKey) (x :| xs) =
    let
      afterLastKey = headMay $ dropWhile (\(k,_) -> k /= lastKey) (x:xs)
      next = fromMaybe x afterLastKey
    in (traceM . show . fst) next >> return next

instance TestEvaluator IO where
  chooseNextThread Nothing ((k,x) :| _) = return (k,x)
  chooseNextThread (Just _) ((k,x) :| _) = return (k,x)

instance TestEvaluator Gen where
  chooseNextThread _ (x :| xs) = do
    n <- Test.QuickCheck.elements (x:xs)
    traceM ("next thread: " <> show (fst n))
    return n

instance (TestEvaluator m, Monad m) => TestEvaluator (StateT s m) where
  chooseNextThread a b = lift $ chooseNextThread a b

instance (Monad m, TestEvaluator m) => TestEvaluator (ReaderT r m) where
  chooseNextThread a b = lift $ chooseNextThread a b

mapInsertUniqueKeyWithSuffix :: Text -> a -> Map Text a -> Map Text a
mapInsertUniqueKeyWithSuffix suffix x m =
  let
    candidateKeys = fmap (Text.append suffix . Text.pack . show) [(1::Int)..]
    usedKeys = Map.keys m
    uniqueKey = head $ dropWhile (\k -> elem k usedKeys) candidateKeys
  in Map.insert uniqueKey x m

isProgramStateIdle :: ProgramState t a -> Bool
isProgramStateIdle ( InternalProgramRunning (_, x)  ) = maybe False (> 2) x
isProgramStateIdle ( ExternalProgramRunning (_, x) ) = maybe False (> 2) x
isProgramStateIdle ( ExternalProgramComplete _ ) = True

isProgramStateActive :: ProgramState t a -> Bool
isProgramStateActive ( InternalProgramRunning (_, x)  ) = maybe False (== 0) x
isProgramStateActive ( ExternalProgramRunning (_, x) ) = maybe False (== 0) x
isProgramStateActive ( ExternalProgramComplete _ ) = False

setIdleCount :: Maybe Int -> ProgramState t a -> ProgramState t a
setIdleCount c (InternalProgramRunning (x, _)) = InternalProgramRunning (x, c)
setIdleCount c (ExternalProgramRunning (x, _)) = ExternalProgramRunning (x, c)
setIdleCount _ (ExternalProgramComplete a) = ExternalProgramComplete a

resetIdleCount :: ProgramState t a -> ProgramState t a
resetIdleCount = setIdleCount Nothing

setAllIdle :: (MonadState (LoopState t r) m) => m ()
setAllIdle = loopStatePrograms %= fmap resetIdleCount

type ActiveCallback m = Monad m => Int -> Bool -> [Text] -> [Text] -> m ()

reportActiveThreads :: (MonadState (LoopState t r) m) => ActiveCallback m -> [(( Text ),ProgramState t r)] -> m ()
reportActiveThreads activeCallback runnableThreads = do
  let active = fst <$> filter (isProgramStateActive . snd) runnableThreads 
  let notIdle = fst <$> filter (not . isProgramStateIdle . snd) runnableThreads 
  inIdleCoolDown <- use loopStateInIdleCoolDown 
  activeCallback 0 inIdleCoolDown active notIdle
  
checkIsComplete :: (MonadState (LoopState t r) m) => [(( Text ),ProgramState t r)] -> m Bool
checkIsComplete runnableThreads  =
  let
    allIdle = getAll $ foldMap (\(_,ps) -> All (isProgramStateIdle ps)) runnableThreads
    anyActive = getAny $ foldMap (\(_,ps) -> Any (isProgramStateActive ps)) runnableThreads
    setCooldownMode x = loopStateInIdleCoolDown .= x
  in do
    LoopState{..} <- get
    if _loopStateInIdleCoolDown then
      if anyActive then do
        setCooldownMode False
        return False
      else
        return allIdle
    else
      if allIdle then do
        setCooldownMode True
        setAllIdle
        return False
      else
        return False

evalMultiDslTest ::
  (Monad m, Functor t, TestEvaluator m) =>
  (Text -> TestCustomCommandStep t m) ->
  ActiveCallback m ->
  EvalState ->
  ExecutionTree (TestProgram t a) ->
  m (ExecutionTree (ThreadResult a))
evalMultiDslTest stepCustomCommand  activeCallback testState threadMap =
    let loopState = buildLoopState threadMap testState
    in evalStateT go loopState
  where
    go  = do
        LoopState {..} <- LazyState.get
        let inCompletePrograms = allIncompletePrograms _loopStatePrograms
        case inCompletePrograms of
            [] -> buildResults
            (x:xs) -> do
              iterationCount <- use loopStateIterations
              if mod iterationCount 1000 == 0 then do
                runnable <- runnablePrograms _loopStatePrograms
                reportActiveThreads (\a b c d -> lift $ activeCallback a b c d) runnable
                isComplete <- checkIsComplete runnable
                if isComplete then
                  buildResults
                else do
                  loopStateIterations %= (+1)
                  go
              else do
                runSomePrograms (x:|xs)
                go
    runSomePrograms runnable = do
      lastRun <- use loopStateLastRan
      nextProgram@(nextProgramKeys, _) <- chooseNextThread lastRun runnable
      loopStateLastRan .= Just nextProgramKeys
      loopStateIterations %= (+1)
      progressThread nextProgram

    progressThread (threadName,p) = do
        (currentThreadUpdate,newThreadUpdate) <- stepEvalThread (\x -> lift $ stepCustomCommand threadName x) p
        let updateCurrentThread =
                updateWithKeyExecutionTree
                    (const currentThreadUpdate)
                    threadName
        loopStatePrograms %= (addSubThread newThreadUpdate . updateCurrentThread)
    addSubThread Nothing tree = tree
    addSubThread (Just (newProgramName,  newProgramState)) (ExecutionTree t) =
        ExecutionTree $
            mapInsertUniqueKeyWithSuffix newProgramName newProgramState
            t

evalDslTest ::
  (Monad m, Functor t, TestEvaluator m) =>
  (Text -> TestCustomCommandStep t m) ->
  Text ->
  F (CustomDsl MemQ.Queue t) a ->
  m a
evalDslTest stepCustomCommand threadName p =
  let
    inputMap = ExecutionTree $ Map.singleton threadName p
    resultSet = evalMultiDslTest stepCustomCommand (\_ _ _ _ -> return ()) emptyEvalState inputMap
  in fromThreadResult . fromJust . (getExecutionTreeEntry threadName) <$> resultSet
