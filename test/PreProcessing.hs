module PreProcessing (testPreProcessing) where

import           Test.Hspec
import           Types (Token(..), BinaryOperator(..), Value(..))
import           Parsing.PreProcessing (applyPreProcessing)

testPreProcessing :: IO ()
testPreProcessing = hspec
  $ do
    it "nothing" $ applyPreProcessing [] `shouldBe` []
    it "no changes"
      $ applyPreProcessing [Sym "hello", Binary Assign, Sym "world"]
      `shouldBe` [Sym "hello", Binary Assign, Sym "world"]
    it "only changes"
      $ applyPreProcessing
        [PreProcessor "pi" [Constant (Float 3.14)], Sym "pi"]
      `shouldBe` [Constant (Float 3.14)]
    it "complex"       $ applyPreProcessing
        [PreProcessor "pi" [Constant (Float 3.14)], Sym "pi", Binary Add, Sym "pi"]
      `shouldBe` [Constant (Float 3.14), Binary Add, Constant (Float 3.14)]
