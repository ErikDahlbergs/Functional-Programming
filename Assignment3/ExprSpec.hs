module ExprSpec where

import Test.Hspec
import Control.Exception (evaluate)

import qualified Dictionary
import Expr

dict = Dictionary.insert ("x", 1) $
       Dictionary.insert ("y", 2) $
       Dictionary.empty 


testValue :: String -> Integer
testValue string = value (fromString string) dict


exprSpec :: Spec
exprSpec = do
  describe "Expr" $ do
    it "should be able to evaluate constants" $ do
      testValue "1" `shouldBe` 1

    it "should be able to evaluate variables" $ do
      testValue "x" `shouldBe` 1

    it "should be able to evaluate addition" $ do
      testValue "x+y" `shouldBe` 3

    it "should be able to evaluate multiplication" $ do
      testValue "x*y" `shouldBe` 2

    it "should be able to evaluate division" $ do
      testValue "12 / 5" `shouldBe` 2

    it "should handle precedence" $ do
      testValue "1 + 2*3" `shouldBe` 7

    it "should be able to evaluate repeated subtraction" $ do
      testValue "x-y-y" `shouldBe` (-3)

    it "shouldn't accept division by zero" $ do
      evaluate (testValue "1/(2-y)") `shouldThrow` anyException  -- error "division by zero") ::

    it "shouldn't allow undefined variables" $ do
      evaluate (testValue "2+z") `shouldThrow` anyException -- error "undefined variable z"


    it "2^3 should be 8" $ do
      testValue "y^3" `shouldBe` 8

    it "2^3^4 should be 2417851639229258349412352" $ do
      testValue "y^3^4" `shouldBe` 2417851639229258349412352

    it "8^4 should be 4096" $ do
      testValue "8^4" `shouldBe` 4096

    it "(y+3)*2^(x+y) should be 40 if x = 1 and y = 2" $ do
      testValue "(y+3)*2^(x+y)" `shouldBe` 40


    it "should be able to calculate 2^3^4+2^5*6+7^8+9" $ do
      testValue "2^3^4+2^5*6+7^8+9" `shouldBe` 2417851639229258355177354
