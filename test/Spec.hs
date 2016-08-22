{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

import DodgerBlue.MyDslExample
import Test.Tasty
import Test.Tasty.Hspec

unitTestSpecs :: (forall a. MyDsl q a -> IO a) -> SpecWith ()
unitTestSpecs dslRunner = do
  describe "evalDslIO" $ do
    it "can write and try read from queue" $
      assertProgramResult (Just 1) writeAndTryRead
    it "can write and read from queue" $
      assertProgramResult 1 writeAndRead
    it "can write in child process and read from queue" $
      assertProgramResult 1 writeFromAChildProcess
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
