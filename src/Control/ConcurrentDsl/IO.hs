{-# LANGUAGE RankNTypes #-}
module Control.ConcurrentDsl.IO
  (evalDslIO)
where

import           Control.ConcurrentDsl.Types
import           Control.Concurrent        (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Free.Church
import           Control.Monad.IO.Class

newQueue :: (MonadIO m) => m (TQueue a)
newQueue = liftIO newTQueueIO

writeQueue :: (MonadIO m) => TQueue a -> a -> m ()
writeQueue q a = (liftIO . atomically) (writeTQueue q a)

tryReadQueue :: (MonadIO m) => TQueue a -> m (Maybe a)
tryReadQueue q = (liftIO . atomically) (tryReadTQueue q)

readQueue :: (MonadIO m) => TQueue a -> m a
readQueue q = (liftIO . atomically) (readTQueue q)

delayMilliseconds :: (MonadIO m) => Int -> m ()
delayMilliseconds milliseconds = (liftIO $ threadDelay (milliseconds * 1000))

type CustomCommandStep t m = forall a. t (m a) -> m a

evalDslIO :: (MonadIO m) =>
  (m () -> IO ()) ->
  CustomCommandStep t m ->
  F (CustomDsl TQueue t) a ->
  m a
evalDslIO runChild stepCustomCommand p = iterM stepProgram p
  where
    stepProgram (DslBase (NewQueue' n)) =
      newQueue >>= n
    stepProgram (DslBase (WriteQueue' q a n)) =
      writeQueue q a >> n
    stepProgram (DslBase (TryReadQueue' q n)) =
      tryReadQueue q >>= n
    stepProgram (DslBase (ReadQueue' q n)) =
      readQueue q >>= n
    stepProgram (DslBase (ForkChild' childProgram n)) = do
      let runner = evalDslIO runChild stepCustomCommand childProgram
      void . liftIO $ async (runChild runner)
      n
    stepProgram (DslBase (Wait' milliseconds n)) =
      delayMilliseconds milliseconds >> n
    stepProgram (DslBase (SetPulseStatus' _status n)) = n -- ignore for now
    stepProgram (DslCustom customCmd) =
      stepCustomCommand customCmd
