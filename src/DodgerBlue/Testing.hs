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
   ThreadGroup(..),
   ThreadResult(..),
   TestProgram,
   ExecutionTree(..),
   ThreadResultGroup(..),
   emptyEvalState)
where

import           Control.Lens
import           Data.Maybe (fromJust, catMaybes)
import           Data.Monoid
import           Control.Monad.Free.Church
import qualified Control.Monad.Free                     as Free
import           Control.Monad.State
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Typeable
import qualified DodgerBlue.InMemoryQueues as MemQ
import           DodgerBlue.Types
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

newtype ExecutionTree a = ExecutionTree { unExecutionTree :: Map Text (Map Text a) }
  deriving (Eq,Show)

instance Arbitrary a => Arbitrary (ExecutionTree a) where
  arbitrary = ExecutionTree <$> arbitrary

instance Foldable ExecutionTree where
  foldMap f (ExecutionTree t) = (foldMap . foldMap) f t

instance Traversable ExecutionTree where
  traverse f (ExecutionTree t) = ExecutionTree <$> (traverse . traverse) f t

instance Functor ExecutionTree where
  fmap f (ExecutionTree t) = ExecutionTree $ (fmap . fmap) f t

mapMaybeExecutionTree :: (a -> Maybe b) -> ExecutionTree a -> ExecutionTree b
mapMaybeExecutionTree f ExecutionTree{..} =
  ExecutionTree $ fmap (Map.mapMaybe f) unExecutionTree

updateWithKeyExecutionTree :: (a -> Maybe a) -> Text -> Text -> ExecutionTree a -> ExecutionTree a
updateWithKeyExecutionTree f nodeName threadName (ExecutionTree t) =
  ExecutionTree $ Map.adjust (Map.update f threadName) nodeName t

getExecutionTreeEntry :: Text -> Text -> ExecutionTree a -> Maybe a
getExecutionTreeEntry node threadName ExecutionTree{..} = do
  threads <- Map.lookup node unExecutionTree
  Map.lookup threadName threads

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

type TestCustomCommandStep t m = forall a. t (TestProgramFree t a) -> m (TestProgramFree t a)

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

data LoopState t a = LoopState {
  loopStatePrograms :: ExecutionTree (ProgramState t a),
  loopStateLastRan :: Maybe (Text,Text),
  loopStateInIdleCoolDown :: Bool }

data StepResult t a =
  StepResultComplete a
  | StepResultContinue (TestProgramFree t a, Maybe Bool)
  | StepResultFork (TestProgramFree t a) (TestProgramFree t ())

standardContinueStep :: TestProgramFree t a -> StepResult t a
standardContinueStep n = StepResultContinue (n, Nothing)

stepProgram :: (MonadState s m, HasDslEvalState s, Monad m, Functor t) => TestCustomCommandStep t m -> TestProgramFree t a -> m (StepResult t a)
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
stepProgram _ (Free.Free (DslBase (ForkChild' childProgram n))) =
  return (StepResultFork n (fromF childProgram))
stepProgram _ (Free.Free (DslBase (SetPulseStatus' active n))) =
  return (StepResultContinue (n, Just active))
stepProgram _ (Free.Free (DslBase _a)) = error $ "todo DslBase _"
stepProgram stepCustomCommand (Free.Free (DslCustom cmd)) =
  stepCustomCommand cmd >>= return . standardContinueStep

buildLoopState :: Functor t => ExecutionTree (TestProgram t a) -> LoopState t a
buildLoopState threadMap =
    LoopState {
      loopStatePrograms = fmap (mkExternalProgramRunning . fromF) threadMap,
      loopStateLastRan = mempty,
      loopStateInIdleCoolDown = False }

updateIdleCount :: Maybe Bool -> Maybe Int -> Maybe Int
updateIdleCount Nothing x = x
updateIdleCount (Just True) _ = Just 0
updateIdleCount (Just False) Nothing = Just 1
updateIdleCount (Just False) (Just x) = Just $ x + 1

stepEvalThread ::
  (MonadState s m, HasDslEvalState s, Functor t) =>
  TestCustomCommandStep t m ->
  ProgramState t a ->
  m (Maybe (ProgramState t a), Maybe (ProgramState t a))
stepEvalThread stepCustomCommand (InternalProgramRunning (p,idleCount)) = do
  result <- stepProgram stepCustomCommand p
  case result of StepResultComplete () -> return $ (Nothing, Nothing)
                 StepResultContinue (n,isActive) -> return $ (Just (InternalProgramRunning (n, updateIdleCount isActive idleCount)), Nothing)
                 StepResultFork n newProgram -> return $ (Just (InternalProgramRunning (n, idleCount)), Just  (mkInternalProgramRunning newProgram))
stepEvalThread stepCustomCommand (ExternalProgramRunning (p,idleCount)) = do
  result <- stepProgram stepCustomCommand p
  case result of StepResultComplete a -> return $ (Just (ExternalProgramComplete a), Nothing)
                 StepResultContinue (n,isActive) -> return $ (Just (ExternalProgramRunning (n, updateIdleCount isActive idleCount)), Nothing)
                 StepResultFork n newProgram -> return $ (Just (ExternalProgramRunning (n,idleCount)), Just (mkInternalProgramRunning newProgram))
stepEvalThread _ (ExternalProgramComplete a) = return (Just (ExternalProgramComplete a), Nothing)

buildResults :: LoopState t a -> ExecutionTree (ThreadResult a)
buildResults LoopState {..} =
  mapMaybeExecutionTree toResult loopStatePrograms
  where
    toResult (InternalProgramRunning _) = Nothing
    toResult (ExternalProgramComplete a) = Just $ ThreadResult a
    toResult (ps@(ExternalProgramRunning _)) =
      if isProgramStateIdle ps then
        Just ThreadIdle
      else
        Just ThreadBlocked

foldlWithKey' :: (s -> Text -> Text -> a -> s) -> s -> ExecutionTree a -> s
foldlWithKey' acc z (ExecutionTree t) = Map.foldlWithKey' (\s nodeName -> Map.foldlWithKey' (\s1 threadName v -> acc s1 nodeName threadName v) s) z t

isProgramBlocked :: (MonadState s m, HasDslEvalState s, Monad m, Functor t) => TestProgramFree t a -> m (Bool)
isProgramBlocked (Free.Free (DslBase (ReadQueue' q _n))) = do
  qs <- use testQueues
  return $ MemQ.isEmptyQueue qs q
isProgramBlocked _ = return False

runnablePrograms :: (MonadState s m, HasDslEvalState s, Monad m, Functor t) => ExecutionTree (ProgramState t a) -> m [(Text,Text,ProgramState t a)]
runnablePrograms  t = (fmap catMaybes) . sequenceA $ foldlWithKey' acc [] t
 where acc xs node threadName programState =
         (justIfRunnable node threadName programState):xs
       justIfRunnable node threadName programState@(ExternalProgramRunning (p,_idleCount)) = do
         isBlocked <- isProgramBlocked p
         if isBlocked then
           return Nothing
         else return $ Just (node, threadName, programState)
       justIfRunnable _ _ (ExternalProgramComplete _) =
         return Nothing
       justIfRunnable node threadName programState@(InternalProgramRunning (p,_idleCount)) = do
         isBlocked <- isProgramBlocked p
         if isBlocked then
           return $ Nothing
         else return $ Just (node, threadName, programState)

chooseThread :: Maybe (Text,Text) -> [a] -> a
chooseThread Nothing (x:_) = x
chooseThread Nothing [] = error "todo chooseThread"
chooseThread (Just _) _  = error "todo chooseThread"

mapInsertUniqueKeyWithSuffix :: Text -> a -> Map Text a -> Map Text a
mapInsertUniqueKeyWithSuffix suffix x m =
  let
    candidateKeys = fmap (Text.append suffix . Text.pack . show) [(1::Int)..]
    usedKeys = Map.keys m
    uniqueKey = head $ dropWhile (\k -> elem k usedKeys) candidateKeys
  in Map.insert uniqueKey x m

isProgramStateIdle :: ProgramState t a -> Bool
isProgramStateIdle ( InternalProgramRunning (_, x)  ) = maybe False (> 0) x
isProgramStateIdle ( ExternalProgramRunning (_, x) ) = maybe False (> 0) x
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

setAllIdle :: LoopState t a -> LoopState t a
setAllIdle s@LoopState{..} = s { loopStatePrograms = fmap resetIdleCount loopStatePrograms }

checkIsComplete :: [(Text,Text,ProgramState t a)] -> LoopState t a -> (Bool, LoopState t a)
checkIsComplete runnableThreads loopState@LoopState{..} =
  let
    allIdle = getAll $ foldMap (\(_,_,ps) -> All (isProgramStateIdle ps)) runnableThreads
    anyActive = getAny $ foldMap (\(_,_,ps) -> Any (isProgramStateActive ps)) runnableThreads
    setCooldownMode x s = s { loopStateInIdleCoolDown = x }
  in
    if loopStateInIdleCoolDown then
      if anyActive then
        (False, setCooldownMode False $ loopState)
      else
        if allIdle then
          (True, loopState)
        else
          (False, loopState)
    else
      if allIdle then
        (False, setAllIdle . setCooldownMode True $ loopState)
      else
        (False, loopState)

evalMultiDslTest ::
  (MonadState s m, HasDslEvalState s, Functor t) =>
  TestCustomCommandStep t m ->
  ExecutionTree (TestProgram t a) ->
  m (ExecutionTree (ThreadResult a))
evalMultiDslTest stepCustomCommand threadMap =
    let loopState = buildLoopState threadMap
    in go loopState
  where
    go loopState@LoopState{..} = do
        runnable <- runnablePrograms loopStatePrograms
        case runnable of
            [] -> return $ buildResults loopState
            xs ->
                let
                  (isComplete, loopState') = checkIsComplete runnable loopState
                  next = chooseThread loopStateLastRan xs
                in
                  if isComplete then
                    return $ buildResults loopState'
                  else
                    progressThread loopState' next
    progressThread loopState@LoopState{..} (node,threadName,p) = do
        (currentThreadUpdate,newThreadUpdate) <-
            stepEvalThread stepCustomCommand p
        let updateCurrentThread =
                updateWithKeyExecutionTree
                    (const currentThreadUpdate)
                    node
                    threadName
        let loopState' =
                loopState
                { loopStatePrograms = ((addSubThread
                                            node
                                            threadName
                                            newThreadUpdate) .
                                       updateCurrentThread)
                      loopStatePrograms
                }
        go loopState'
    addSubThread _ _ Nothing tree = tree
    addSubThread node threadName (Just newProgramState) (ExecutionTree t) =
        ExecutionTree $
        Map.adjust
            (mapInsertUniqueKeyWithSuffix threadName newProgramState)
            node
            t

evalDslTest ::
  (MonadState s m, HasDslEvalState s, Functor t) =>
  TestCustomCommandStep t m ->
  F (CustomDsl MemQ.Queue t) a ->
  m a
evalDslTest stepCustomCommand p =
  let
    mainThreadKey = "MainThread"
    inputMap = ExecutionTree $ Map.singleton mainThreadKey (Map.singleton mainThreadKey p)
    resultSet = evalMultiDslTest stepCustomCommand inputMap
  in
    fromThreadResult . fromJust . (getExecutionTreeEntry mainThreadKey mainThreadKey) <$> resultSet
