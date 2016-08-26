{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

module DodgerBlue.Types
  (CustomDsl(..),
   ConcurrentDslCmd(..),
   CustomCommandStep) where

import Data.Typeable
import Control.Monad.Free.Church

data ConcurrentDslCmd q d next where
  NewQueue' :: Typeable a => (q a -> next) -> ConcurrentDslCmd q d next
  WriteQueue' :: Typeable a => q a -> a -> next -> ConcurrentDslCmd q d next
  TryReadQueue' :: Typeable a => q a ->  (Maybe a -> next) -> ConcurrentDslCmd q d next
  ReadQueue' :: Typeable a => q a ->  (a -> next) -> ConcurrentDslCmd q d next
  ForkChild' :: F (CustomDsl q d) () -> next -> ConcurrentDslCmd q d next
  SetPulseStatus' :: Bool -> next -> ConcurrentDslCmd q d next

deriving instance Functor (ConcurrentDslCmd q d)

data CustomDsl q d next =
  DslBase (ConcurrentDslCmd q d next)
  | DslCustom (d next)

instance Functor d => Functor (CustomDsl q d) where
  fmap f (DslBase a) = DslBase $ fmap f a
  fmap f (DslCustom a) = DslCustom $ fmap f a

type CustomCommandStep t m = forall a. t (m a) -> m a
