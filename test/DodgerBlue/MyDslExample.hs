{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE OverloadedStrings      #-}

module DodgerBlue.MyDslExample
  (writeAndTryRead
  ,writeAndRead
  ,writeFromAChildProcess
  ,DodgerBlue.MyDslExample.wait
  ,readForever
  ,idleForever
  ,forkChildAndExit
  ,forkChildAndWaitForResult
  ,MyDsl
  ,MonadMyDsl
  ,MyDslFunctions
  ,myEvalIO
  ,myEvalTest
  ,myEvalMultiDslTest
  ,myEvalMultiDslTestGen)
  where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Resource
import           Control.Concurrent.Async hiding (wait)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM
import           Control.Monad.Identity
import           Control.Monad.Free.Church
import           Data.Text (Text)
import           Data.Typeable
import           Data.Foldable
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
    wait :: Int -> m ()
    forkChild :: m () -> m ()
    setPulseStatus :: Bool -> m ()

forkChildIO :: ResourceT IO () -> ResourceT IO ()
forkChildIO c = do
  _ <- allocate (async (runResourceT c)) cancel
  return ()

instance MonadMyDsl (ResourceT IO) where
    type QueueType (ResourceT IO) = TQueue
    newQueue = DslIO.newQueue
    writeQueue = DslIO.writeQueue
    readQueue = DslIO.readQueue
    tryReadQueue = DslIO.tryReadQueue
    forkChild = forkChildIO
    wait = lift . waitIO
    setPulseStatus _ = return ()

instance MonadMyDsl (F (DodgerBlue.CustomDsl q MyDslFunctions)) where
    type QueueType (F (DodgerBlue.CustomDsl q MyDslFunctions)) = q
    newQueue = DodgerBlue.newQueue
    writeQueue = DodgerBlue.writeQueue
    readQueue = DodgerBlue.readQueue
    tryReadQueue = DodgerBlue.tryReadQueue
    forkChild = DodgerBlue.forkChild
    setPulseStatus = DodgerBlue.setPulseStatus
    wait = waitFree

waitFree
    :: (MonadFree (DodgerBlue.CustomDsl q MyDslFunctions) m)
    => Int -> m ()
waitFree seconds = DodgerBlue.Testing.customCmd $ MyDslWait seconds ()

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

forkChildAndExit
    :: MonadMyDsl m
    =>
    QueueType m Int
    -> Int
    -> m ()
forkChildAndExit q waitTime = do
  forkChild (childThread q)
  wait waitTime
  return ()
  where
    childThread childQueue = forever $ do
      wait 1
      writeQueue childQueue 1

forkChildAndWaitForResult
    :: MonadMyDsl m
    =>
    m Int
forkChildAndWaitForResult = do
  q <- newQueue
  signalQ <- newQueue
  forkChild (childThread q signalQ)
  traverse_ (writeQueue q) [(1::Int)..100]
  readQueue signalQ
  where
    childThread childQ childSignalQ = go 0
      where
        go 5050 = writeQueue childSignalQ 5050
        go acc = do
          r <- readQueue childQ
          go (acc + r)

waitIO :: Int -> IO ()
waitIO milliseconds = threadDelay (milliseconds * 1000)

runMyDslFunctionIO :: MyDslFunctions (IO a) -> IO a
runMyDslFunctionIO (MyDslWait milliseconds n) = waitIO milliseconds  >> n

runMyDslFunctionTest :: Monad m => Text -> MyDslFunctions (a) -> m a
runMyDslFunctionTest _threadName (MyDslWait _milliseconds n) = return n

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
          (\_ _ _ _ -> return ())
          DodgerBlue.Testing.emptyEvalState
          programs)

myEvalMultiDslTestGen ::
  DodgerBlue.Testing.ExecutionTree (DodgerBlue.Testing.TestProgram MyDslFunctions a)
  -> Gen (DodgerBlue.Testing.ExecutionTree (DodgerBlue.Testing.ThreadResult a))
myEvalMultiDslTestGen programs =
    DodgerBlue.Testing.evalMultiDslTest
          runMyDslFunctionTest
          (\_ _ _ _ -> return ())
          DodgerBlue.Testing.emptyEvalState
          programs

myEvalTest :: MyDsl DodgerBlue.Testing.Queue a -> IO a
myEvalTest p = do
  DodgerBlue.Testing.evalDslTest runMyDslFunctionTest "MainThread" p
