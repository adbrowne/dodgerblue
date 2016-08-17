{-# LANGUAGE FlexibleContexts #-}
module Control.ConcurrentDsl
    ( evalDslIO,
      forkChild,
      newQueue,
      writeQueue,
      tryReadQueue,
      wait,
      setPulseStatus,
      CustomDsl) where

import           Data.Typeable
import           Control.Monad.Free.Church
import           Control.ConcurrentDsl.IO
import           Control.ConcurrentDsl.Types

forkChild :: (Functor d, MonadFree (CustomDsl q d) m) => F (CustomDsl q d) () -> m ()
forkChild p = liftF . DslBase $ ForkChild' p ()

newQueue :: (Functor d, MonadFree (CustomDsl q d) m, Typeable a) => m (q a)
newQueue = liftF . DslBase $ NewQueue' id

writeQueue :: (Functor d, MonadFree (CustomDsl q d) m, Typeable a) => q a -> a -> m ()
writeQueue queue item = liftF . DslBase $ WriteQueue' queue item ()

tryReadQueue :: (Functor d, MonadFree (CustomDsl q d) m, Typeable a) => q a -> m (Maybe a)
tryReadQueue queue = liftF . DslBase $ TryReadQueue' queue id

wait :: (Functor d, MonadFree (CustomDsl q d) m) => Int -> m ()
wait seconds = liftF . DslBase $ Wait' seconds ()

setPulseStatus :: (Functor d, MonadFree (CustomDsl q d) m) => Bool -> m ()
setPulseStatus active = liftF . DslBase $ SetPulseStatus' active ()
