{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Concurrent.STM
import DodgerBlue
import DodgerBlue.Testing
import Test.Tasty
import           Control.Monad.State
import Control.Monad.Free.Church
import Test.Tasty.Hspec

data MyDslFunctions next = MyDslFunctions next
  deriving Functor

type MyDsl q = F (CustomDsl q MyDslFunctions)

writeAndTryRead :: MyDsl q (Maybe Int)
writeAndTryRead = do
  q <- newQueue
  writeQueue q 1
  tryReadQueue q

writeAndRead :: MyDsl q Int
writeAndRead = do
  q <- newQueue
  writeQueue q 1
  readQueue q

myEvalIO ::
  MyDsl TQueue a
  -> IO a
myEvalIO = evalDslIO id (\(MyDslFunctions n) -> n)

data TestState = TestState {
  _testStateQueues :: Queues }

$(makeLenses ''TestState)

instance HasTestQueues TestState where
  testQueues = testStateQueues

myEvalTest ::
  MyDsl Queue a
  -> IO a
myEvalTest p = return $ evalState (evalDslTest (\(MyDslFunctions n) -> n) p) initialState
  where
    initialState = TestState { _testStateQueues = emptyQueues }

unitTestSpecs :: (forall a. MyDsl q a -> IO a) -> SpecWith ()
unitTestSpecs dslRunner = do
  describe "evalDslIO" $ do
    it "can write and try read from queue" $
      assertProgramResult (Just 1) writeAndTryRead
    it "can write and read from queue" $
      assertProgramResult 1 writeAndRead
  where
    assertProgramResult expected program = do
      result <- dslRunner program
      result `shouldBe` expected

main :: IO ()
main = do
  unitTestSpecsIO <- testSpec "Unit tests - IO" (unitTestSpecs myEvalIO)
  unitTestSpecsTest <- testSpec "Unit tests - Test" (unitTestSpecs myEvalTest)
  defaultMain $ testGroup "Tests" [
    unitTestSpecsIO,
    unitTestSpecsTest ]
