{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Monoid
import           Data.Text               (Text)
import           DodgerBlue.MyDslExample
import           Test.QuickCheck.Instances ()
import           DodgerBlue.Testing
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.QuickCheck

unitTestSpecs
    :: MonadMyDsl m
    => (forall a. m a -> IO a) -> SpecWith ()
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
              input = ExecutionTree $ Map.singleton "main" (Map.singleton "main" readForever)
              result = myEvalMultiDslTest input
              expected = ExecutionTree $ Map.singleton "main" (Map.singleton "main" (ThreadBlocked))
            in result `shouldBe` expected
    it "idle program returns idle result" $
            let
              input = ExecutionTree $ Map.singleton "main" (Map.singleton "main" idleForever)
              result = myEvalMultiDslTest input
              expected = ExecutionTree $ Map.singleton "main" (Map.singleton "main" (ThreadIdle))
            in result `shouldBe` expected

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

main :: IO ()
main = do
    unitTestSpecsIO <- testSpec "Unit tests - IO" (unitTestSpecs myEvalIO)
    unitTestSpecsTest <- testSpec "Unit tests - Test" (unitTestSpecs myEvalTest)
    unitTestSpecsNoFree <- testSpec "Unit tests - NoFree" (unitTestSpecs id)
    unitTestTestInterpreter <- testSpec "Unit tests - test interpreter" testInterpreterUnitTests
    defaultMain $
        testGroup
            "Tests"
            [ unitTestSpecsIO
            , unitTestSpecsTest
            , unitTestSpecsNoFree
            , unitTestTestInterpreter 
            , testProperty 
                  "All programs have an output"
                  prop_allProgramsHaveAnOutput] 
