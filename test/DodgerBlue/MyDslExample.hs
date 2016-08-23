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
  ,MyDsl
  ,MonadMyDsl
  ,MyDslFunctions
  ,myEvalIO
  ,myEvalTest
  ,myEvalMultiDslTest)
  where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.Free.Church
import           Control.Monad.State
import qualified Data.Map.Strict           as Map
import           Data.Text                 (Text)
import           Data.Typeable
import qualified DodgerBlue
import qualified DodgerBlue.IO             as DslIO
import qualified DodgerBlue.Testing

data MyDslFunctions next =
    MyDslFunctions next
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

instance MonadMyDsl IO where
    type QueueType IO = TQueue
    newQueue = DslIO.newQueue
    writeQueue = DslIO.writeQueue
    readQueue = DslIO.readQueue
    tryReadQueue = DslIO.tryReadQueue
    forkChild c = (void . async) c

instance MonadMyDsl (F (DodgerBlue.CustomDsl q MyDslFunctions)) where
    type QueueType (F (DodgerBlue.CustomDsl q MyDslFunctions)) = q
    newQueue = DodgerBlue.newQueue
    writeQueue = DodgerBlue.writeQueue
    readQueue = DodgerBlue.readQueue
    tryReadQueue = DodgerBlue.tryReadQueue
    forkChild = DodgerBlue.forkChild

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

myEvalIO :: MyDsl TQueue a -> IO a
myEvalIO =
    DodgerBlue.evalDslIO
        id
        (\(MyDslFunctions n) ->
              n)

myEvalMultiDslTest ::
  Map.Map Text (DodgerBlue.Testing.ThreadGroup MyDslFunctions a)
  -> Map.Map Text (DodgerBlue.Testing.ThreadResultGroup a)
myEvalMultiDslTest programs =
    evalState
        (DodgerBlue.Testing.evalMultiDslTest
             (\(MyDslFunctions n) ->
                   return n)
             programs)
        DodgerBlue.Testing.emptyEvalState

myEvalTest :: MyDsl DodgerBlue.Testing.Queue a -> IO a
myEvalTest p =
    return $
    evalState
        (DodgerBlue.Testing.evalDslTest
             (\(MyDslFunctions n) ->
                   return n)
             p)
        DodgerBlue.Testing.emptyEvalState
