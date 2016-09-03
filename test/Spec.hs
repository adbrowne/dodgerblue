{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

import qualified Data.Map.Strict         as Map
import           Data.Monoid
import           Control.Monad.Trans.Resource
import           Control.Concurrent.STM as STM
import           Control.Concurrent (threadDelay)
import           DodgerBlue.MyDslExample
import           DodgerBlue
import           Test.QuickCheck.Instances ()
import           DodgerBlue.Testing
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.QuickCheck

unitTestSpecs
    :: (MonadMyDsl m)
    => (forall a. Show a => m a -> IO a) -> SpecWith ()
unitTestSpecs dslRunner = do
    describe "all evaluators unit tests" $
        do it "can write and try read from queue" $
               assertProgramResult (Just 1) writeAndTryRead
           it "can write and read from queue" $
               assertProgramResult 1 writeAndRead
           it "can write in child process and read from queue" $
               assertProgramResult 1 writeFromAChildProcess
  where
    assertProgramResult expected program = do
        result <- dslRunner program
        result `shouldBe` expected

testInterpreterUnitTests :: SpecWith ()
testInterpreterUnitTests = do
  describe "test interpreter unit tests" $ do
    it "blocked program returns blocked result" $
            let
              input = ExecutionTree $ Map.singleton "main" readForever
              result = myEvalMultiDslTest input
              expected = ExecutionTree $ Map.singleton "main" (ThreadBlocked)
            in result `shouldBe` expected
    it "idle program returns idle result" $
            let
              input = ExecutionTree $ Map.singleton "main" idleForever
              result = myEvalMultiDslTest input
              expected = ExecutionTree $ Map.singleton "main" (ThreadIdle)
            in result `shouldBe` expected

childThreadShouldBlowUpParent :: SpecWith ()
childThreadShouldBlowUpParent =
  it "When the child thread blows up so should the parent" $ do
      myEvalIO childBlowsUp `shouldThrow` (errorCall "child thread blowing up")
  where
    childThread = do
      wait 3
      error "child thread blowing up"
    childBlowsUp = do
      DodgerBlue.forkChild childThread
      wait 1000
      _ <- error "parent thread blew up" -- shouldn't happen as the child thread should blow up first
      return (1 :: Int)

delay :: Int -> IO ()
delay milliseconds = threadDelay (milliseconds * 1000)

drainQueue :: TQueue a -> STM [a]
drainQueue q =
  go []
  where
    go acc = do
      readResult <- STM.tryReadTQueue q
      maybe (return acc) (\x -> go (x:acc)) readResult


parentThreadShouldKillChild :: SpecWith ()
parentThreadShouldKillChild =
  it "When a parent thread exits the child should stop" $ do
      (q :: TQueue Int) <- newTQueueIO
      let parentWaitTime = 120
      _ <- runResourceT (forkChildAndExit q parentWaitTime)
      delay 300
      queueLength <- length <$> atomically (drainQueue q)
      queueLength `shouldSatisfy` (\x -> x > 0 && x < parentWaitTime + 50)

childThreadShouldContinueRunningWhileParentThreadIs :: SpecWith ()
childThreadShouldContinueRunningWhileParentThreadIs =
  it "While the parent thread is running the child thread should run" $ do
      result <- runResourceT forkChildAndWaitForResult
      result `shouldBe` 5050

ioUnitTests :: SpecWith ()
ioUnitTests = do
  describe "IO runner" $ do
    childThreadShouldBlowUpParent
    parentThreadShouldKillChild
    childThreadShouldContinueRunningWhileParentThreadIs

data MyDslProgram = MyDslProgram { myDslProgramName :: String, unMyDslProgram :: MyDsl Queue Int }

instance Show MyDslProgram where
  show a = "dsl program: " <> myDslProgramName a

genProgramSet :: Gen (ExecutionTree MyDslProgram)
genProgramSet = do
    (structure :: ExecutionTree ()) <- arbitrary
    let (childProgram :: MyDslProgram) = MyDslProgram "writeFromAChildProcess" writeFromAChildProcess
    (structureWithPrograms :: ExecutionTree MyDslProgram) <- traverse (const $ return childProgram) structure
    return structureWithPrograms

prop_allProgramsHaveAnOutput :: Property
prop_allProgramsHaveAnOutput =
    let programInput ps = (const ()) <$> ps
        programOutput ps = (const ()) <$> (myEvalMultiDslTest (unMyDslProgram <$> ps))
        programInputAndOutputTheSame ps = programInput ps === programOutput ps
    in forAll (resize 10 genProgramSet) programInputAndOutputTheSame

prop_CanRunTestsInsideGenMonad :: Property
prop_CanRunTestsInsideGenMonad =
    let programInput = ExecutionTree $ Map.singleton "MyThread" writeAndRead
        expected = ExecutionTree $ Map.singleton "MyThread" (ThreadResult (1 :: Int))
        resultIsOne result = result === expected
    in forAll (myEvalMultiDslTestGen programInput) resultIsOne

main :: IO ()
main = do
    unitTestSpecsIO <- testSpec "Unit tests - IO" (unitTestSpecs myEvalIO)
    unitTestSpecsTest <- testSpec "Unit tests - Test" (unitTestSpecs myEvalTest)
    unitTestSpecsNoFree <- testSpec "Unit tests - NoFree" (unitTestSpecs runResourceT)
    unitTestTestInterpreter <- testSpec "Unit tests - test interpreter" testInterpreterUnitTests
    unitTestIO <- testSpec "Unit tests - IO" ioUnitTests
    defaultMain $
        testGroup
            "Tests"
            [ unitTestSpecsIO
            , unitTestSpecsTest
            , unitTestSpecsNoFree
            , unitTestTestInterpreter 
            , unitTestIO
            , testProperty 
                  "All programs have an output"
                  prop_allProgramsHaveAnOutput
            , testProperty 
                  "Can run tests inside Gen monad"
                  prop_CanRunTestsInsideGenMonad] 
