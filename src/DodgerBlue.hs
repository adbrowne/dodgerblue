{-# LANGUAGE FlexibleContexts #-}

module DodgerBlue
  (evalDslIO
  ,forkChild
  ,newQueue
  ,writeQueue
  ,tryReadQueue
  ,readQueue
  ,setPulseStatus
  ,CustomDsl)
  where

import Data.Text (Text)
import Data.Typeable
import Control.Monad.Free.Church
import DodgerBlue.IO (evalDslIO)
import DodgerBlue.Types

forkChild
    :: (Functor d, MonadFree (CustomDsl q d) m)
    => Text -> F (CustomDsl q d) () -> m ()
forkChild name p = liftF . DslBase $ ForkChild' name p ()

newQueue
    :: (Functor d, MonadFree (CustomDsl q d) m, Typeable a)
    => m (q a)
newQueue = liftF . DslBase $ NewQueue' id

writeQueue
    :: (Functor d, MonadFree (CustomDsl q d) m, Typeable a)
    => q a -> a -> m ()
writeQueue queue item = liftF . DslBase $ WriteQueue' queue item ()

tryReadQueue
    :: (Functor d, MonadFree (CustomDsl q d) m, Typeable a)
    => q a -> m (Maybe a)
tryReadQueue queue = liftF . DslBase $ TryReadQueue' queue id

readQueue
    :: (Functor d, MonadFree (CustomDsl q d) m, Typeable a)
    => q a -> m a
readQueue queue = liftF . DslBase $ ReadQueue' queue id

setPulseStatus
    :: (Functor d, MonadFree (CustomDsl q d) m)
    => Bool -> m ()
setPulseStatus active = liftF . DslBase $ SetPulseStatus' active ()
