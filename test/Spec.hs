{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
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
  describe "test interpreter unit tests" $
      do it "blocked program returns blocked result" $
            let
              input = Map.singleton "main" (ThreadGroup $ Map.singleton "main" readForever)
              result = myEvalMultiDslTest input
              expected = Map.singleton "main" (ThreadResultGroup $ Map.singleton "main" (ThreadBlocked))
            in result `shouldBe` expected

genProgramSet :: Gen (Map Text (ThreadGroup MyDslFunctions Int))
genProgramSet = do
    (structure :: Map Text (Map Text ())) <- arbitrary
    let (childProgram :: MyDsl Queue Int) = writeFromAChildProcess
    (structureWithPrograms :: Map Text (Map Text (MyDsl Queue Int))) <- mapM (mapM (const $ return childProgram)) structure
    let (result :: Map Text (ThreadGroup MyDslFunctions Int)) = fmap ThreadGroup structureWithPrograms
    return result

prop_allProgramsHaveAnOutput :: Property
prop_allProgramsHaveAnOutput =
    let programInput ps = (Map.keys . threadGroupPrograms) <$> ps
        programOutput ps = (Map.keys . threadResultGroupPrograms) <$> (myEvalMultiDslTest ps)
        programInputAndOutputTheSame ps = programInput ps === programOutput ps
    in forAll genProgramSet programInputAndOutputTheSame

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
