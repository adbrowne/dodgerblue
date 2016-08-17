{-# LANGUAGE DeriveFunctor #-}
import Control.Concurrent.STM
import Control.ConcurrentDsl
import Test.Tasty
import Control.Monad.Free.Church
import Test.Tasty.Hspec

data MyDslFunctions next = MyDslFunctions next
  deriving Functor

type MyDsl q = F (CustomDsl q MyDslFunctions)

myProgram :: MyDsl q (Maybe Int)
myProgram = do
  q <- newQueue
  writeQueue q 1
  tryReadQueue q

myEvalIO ::
  F (CustomDsl TQueue MyDslFunctions) a
  -> IO a
myEvalIO = evalDslIO id (\(MyDslFunctions n) -> n)

main :: IO ()
main = do
  spec1 <- testSpec "Unit tests" $ do
      describe "evalDslIO" $ do
        it "can write and read from queue" $ do
          result <- myEvalIO myProgram
          result `shouldBe`Just 1

  defaultMain $ testGroup "Tests" [ spec1 ]
