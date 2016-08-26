{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module DodgerBlue.InMemoryQueues
  (Queues
  ,Queue
  ,emptyQueues
  ,isEmptyQueue
  ,newQueue
  ,writeToQueue
  ,tryReadFromQueue)
  where

import           Data.Dynamic
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (isJust)
import           Safe            (fromJustNote)
import           Data.Sequence   (ViewR (..), (<|))
import qualified Data.Sequence   as Seq

data Queues = Queues
    { queuesNextIndex :: Int
    , queuesQueueMap  :: Map Int (Maybe Dynamic)
    } deriving ((Show))

newtype Queue a = Queue
    { unQueue :: Int
    } deriving ((Show))

emptyQueues :: Queues
emptyQueues =
    Queues
    { queuesNextIndex = 0
    , queuesQueueMap = mempty
    }

newQueue :: Queues -> (Queue a, Queues)
newQueue Queues{..} =
    let queueKey =
            Queue
            { unQueue = queuesNextIndex
            }
        queues =
            Queues
            { queuesNextIndex = queuesNextIndex + 1
            , queuesQueueMap = Map.insert
                  queuesNextIndex
                  Nothing
                  queuesQueueMap
            }
    in (queueKey, queues)

writeToQueue
    :: Typeable a
    => Queues -> Queue a -> a -> Queues
writeToQueue queues@Queues{..} Queue{..} item =
    let q = fromJustNote "writeToQueue: could not find queue" $ Map.lookup unQueue queuesQueueMap
        updatedQueue = addItem q
    in queues
       { queuesQueueMap = Map.insert unQueue (Just updatedQueue) queuesQueueMap
       }
  where
    addItem Nothing = toDyn $ Seq.singleton item
    addItem (Just dyn) =
        let items =
                fromDyn
                    dyn
                    (error
                         "InMemoryQueues.writeToQueue Invalid format for queue")
        in toDyn $ item <| items

peekQueue :: Typeable a => Queues -> Queue a -> Maybe a
peekQueue Queues{..} Queue{..} =
    let q = fromJustNote "peekQueue: could not find queue" $ Map.lookup unQueue queuesQueueMap
        (maybeHead) = tryRead q
    in maybeHead
  where
    tryRead Nothing = (Nothing)
    tryRead (Just dyn) =
        let items =
                fromDyn
                    dyn
                    (error
                         "InMemoryQueues.peekQueue Invalid format for queue")
            (item) = tryReadSeq items
        in (item)
    tryReadSeq (Seq.viewr -> Seq.EmptyR) = (Nothing)
    tryReadSeq (Seq.viewr -> _xs :> x) = (Just x)
    tryReadSeq _ = error "match error on Seq"

isEmptyQueue
    :: Typeable a
    => Queues -> Queue a -> Bool
isEmptyQueue queues q =
    not . isJust $ peekQueue queues q

tryReadFromQueue
    :: Typeable a
    => Queues -> Queue a -> (Maybe a, Queues)
tryReadFromQueue queues@Queues{..} Queue{..} =
    let q = fromJustNote "tryReadFromQueue: could not find queue" $ Map.lookup unQueue queuesQueueMap
        (maybeItem,updatedQueue) = tryRead q
    in ( maybeItem
       , queues
         { queuesQueueMap = Map.insert unQueue updatedQueue queuesQueueMap
         })
  where
    tryRead Nothing = (Nothing, Nothing)
    tryRead (Just dyn) =
        let items =
                fromDyn
                    dyn
                    (error
                         "InMemoryQueues.tryReadFromQueue Invalid format for queue")
            (item,queue') = tryReadSeq items
        in (item, toDyn <$> queue')
    tryReadSeq (Seq.viewr -> Seq.EmptyR) = (Nothing, Nothing)
    tryReadSeq (Seq.viewr -> xs :> x) = (Just x, Just xs)
    tryReadSeq _ = error "match error on Seq"
