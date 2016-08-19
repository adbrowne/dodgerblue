{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DodgerBlue.Testing
  (evalDslTest,
   HasTestQueues(..),
   MemQ.Queue,
   MemQ.Queues,
   MemQ.emptyQueues)
where

import           Control.Lens
import           Control.Monad.Free.Church
import qualified Control.Monad.Free                     as Free
import           Control.Monad.State
import           Data.Typeable
import qualified DodgerBlue.InMemoryQueues as MemQ
import           DodgerBlue.Types

runWriteQueueCmd :: (MonadState s m, HasTestQueues s, Typeable a) => MemQ.Queue a -> a -> m ()
runWriteQueueCmd q x = do
  testQueues %= (\qs -> MemQ.writeToQueue qs q x)

runTryReadQueueCmd :: (MonadState s m, HasTestQueues s, Typeable a) => MemQ.Queue a -> m (Maybe a)
runTryReadQueueCmd q = do
  qs <- use testQueues
  let (x, qs') = MemQ.tryReadFromQueue qs q
  testQueues .= qs'
  return x

runNewQueueCmd :: (MonadState s m, HasTestQueues s, Typeable a) => m (MemQ.Queue a)
runNewQueueCmd = do
  qs <- use testQueues
  let (x, qs') = MemQ.newQueue qs
  testQueues .= qs'
  return x

class HasTestQueues a where
  testQueues :: Lens' a MemQ.Queues

type TestCustomCommandStep t m = forall a. t (Free.Free (CustomDsl MemQ.Queue t) a) -> m (Free.Free (CustomDsl MemQ.Queue t) a)

evalDslTest ::
  (MonadState s m, HasTestQueues s, Functor t) =>
  TestCustomCommandStep t m ->
  F (CustomDsl MemQ.Queue t) a ->
  m a
evalDslTest stepCustomCommand p = go (fromF p)
  where
    go (Free.Pure a) = return a
    go (Free.Free (DslBase (NewQueue' n))) =
      runNewQueueCmd >>= go . n
    go (Free.Free (DslBase (WriteQueue' q a n))) =
      runWriteQueueCmd q a >> go n
    go (Free.Free (DslBase (TryReadQueue' q n))) =
      runTryReadQueueCmd q >>= go . n
    go cmd@(Free.Free (DslBase (ReadQueue' q n))) = do
      result <- runTryReadQueueCmd q
      case result of Just a  -> go (n a)
                     Nothing -> go cmd
    go (Free.Free (DslCustom cmd)) = do
      result <- stepCustomCommand cmd
      go result
    go (Free.Free (DslBase _)) = undefined
