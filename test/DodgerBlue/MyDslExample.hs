{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}

module DodgerBlue.MyDslExample
  (writeAndTryRead
  ,writeAndRead
  ,writeFromAChildProcess
  ,readForever
  ,idleForever
  ,MyDsl
  ,MonadMyDsl
  ,MyDslFunctions
  ,myEvalIO
  ,myEvalTest
  ,myEvalMultiDslTest
  ,myEvalMultiDslTestGen)
  where

import           Control.Concurrent.Async
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM
import           Control.Monad.Identity
import           Control.Monad.Free.Church
import           Data.Typeable
import qualified DodgerBlue
import qualified DodgerBlue.IO             as DslIO
import qualified DodgerBlue.Testing
import           Test.QuickCheck

data MyDslFunctions next =
    MyDslWait Int next
    deriving (Functor)

type MyDsl q = F (DodgerBlue.CustomDsl q MyDslFunctions)

class Monad m =>
      MonadMyDsl m  where
    type QueueType m :: * -> *
    newQueue
        :: forall a.
           Typeable a
        => m (QueueType m a)
    writeQueue
        :: forall a.
           Typeable a
        => QueueType m a -> a -> m ()
    readQueue
        :: forall a.
           Typeable a
        => QueueType m a -> m a
    tryReadQueue
        :: forall a.
           Typeable a
        => QueueType m a -> m (Maybe a)
    forkChild :: m () -> m ()
    setPulseStatus :: Bool -> m ()

instance MonadMyDsl IO where
    type QueueType IO = TQueue
    newQueue = DslIO.newQueue
    writeQueue = DslIO.writeQueue
    readQueue = DslIO.readQueue
    tryReadQueue = DslIO.tryReadQueue
    forkChild c = (void . async) c
    setPulseStatus _ = return ()

instance MonadMyDsl (F (DodgerBlue.CustomDsl q MyDslFunctions)) where
    type QueueType (F (DodgerBlue.CustomDsl q MyDslFunctions)) = q
    newQueue = DodgerBlue.newQueue
    writeQueue = DodgerBlue.writeQueue
    readQueue = DodgerBlue.readQueue
    tryReadQueue = DodgerBlue.tryReadQueue
    forkChild = DodgerBlue.forkChild
    setPulseStatus = DodgerBlue.setPulseStatus

writeAndTryRead
    :: MonadMyDsl m
    => m (Maybe Int)
writeAndTryRead = do
    q <- newQueue
    writeQueue q 1
    tryReadQueue q

writeAndRead
    :: MonadMyDsl m
    => m Int
writeAndRead = do
    q <- newQueue
    writeQueue q 1
    readQueue q

writeFromAChildProcess
    :: MonadMyDsl m
    => m Int
writeFromAChildProcess = do
    q <- newQueue
    forkChild (writeQueue q 1)
    readQueue q

readForever
    :: MonadMyDsl m
    => m Int
readForever = do
    q <- newQueue
    readQueue q

idleForever
    :: MonadMyDsl m
    => m ()
idleForever = forever $ setPulseStatus False

runMyDslFunctionIO :: MyDslFunctions (IO a) -> IO a
runMyDslFunctionIO (MyDslWait seconds n) = threadDelay (seconds * 10000) >> n

runMyDslFunctionTest :: Monad m => MyDslFunctions (a) -> m a
runMyDslFunctionTest (MyDslWait _seconds n) = return n

myEvalIO :: MyDsl TQueue a -> IO a
myEvalIO =
    DodgerBlue.evalDslIO
        id
        runMyDslFunctionIO

myEvalMultiDslTest ::
  DodgerBlue.Testing.ExecutionTree (DodgerBlue.Testing.TestProgram MyDslFunctions a)
  -> DodgerBlue.Testing.ExecutionTree (DodgerBlue.Testing.ThreadResult a)
myEvalMultiDslTest programs =
    runIdentity $ (DodgerBlue.Testing.evalMultiDslTest
          runMyDslFunctionTest
          DodgerBlue.Testing.emptyEvalState
          programs)

myEvalMultiDslTestGen ::
  DodgerBlue.Testing.ExecutionTree (DodgerBlue.Testing.TestProgram MyDslFunctions a)
  -> Gen (DodgerBlue.Testing.ExecutionTree (DodgerBlue.Testing.ThreadResult a))
myEvalMultiDslTestGen programs =
    DodgerBlue.Testing.evalMultiDslTest
          runMyDslFunctionTest
          DodgerBlue.Testing.emptyEvalState
          programs

myEvalTest :: MyDsl DodgerBlue.Testing.Queue a -> IO a
myEvalTest p = do
  DodgerBlue.Testing.evalDslTest runMyDslFunctionTest p
