module LibSpec (spec) where

import           Lib
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "Divide Function" $ do

    it "returns Just 0 for 0/x" $ do
      (divideSafe 0 3) == Just 0  `shouldBe` True

    it "returns Just 5 for 10/2" $ do
      (divideSafe 10 2) `shouldBe` Just 5

    it "returns Nothing for x/0" $ do
      (divideSafe 10 0) `shouldBe` Nothing

    -- some time after development starts we decide that we want
    -- to return nothing if x > 100. This might arise because
    -- stakeholders decide its a new feature requirement.
    --  We add test cases for it.
    -- Two that come to mind are to test that out code (that we
    -- have not yet rewritten, will still work with x > 100 and y = 0.
    -- A second case is x > 100 and y > 0 - the nortmal case.
    it "should return nothing for x > 100, y == 0" $ do
      (divideSafe 101 0) `shouldBe` Nothing

    it "should return nothing for x > 100, y == 10" $ do
      (divideSafe 101 10) `shouldBe` Nothing

  describe "Validate function for Infix Expressions" $ do
    it "Should return invalid for (30 + 5)2" $ do
      infixValidator ["(", "30", "+", "5", ")", "2"] `shouldBe` False
    
    it "Should return valid for (30 + 5) + 2" $ do
      infixValidator ["(", "30", "+", "5", ")", "+", "2"] `shouldBe` True
    
    it "Should return invalid for (30 + 5) + )" $ do
      infixValidator ["(", "30", "+", "5", ")", "+", ")"] `shouldBe` False
    
    it "Should return invalid for (30 + 5) + 2)" $ do
      infixValidator ["(", "30", "+", "5", ")", "+", "2", ")"] `shouldBe` False

    it "Should return valid for (30 + 5) + 2" $ do
      infixValidator ["(", "30", "+", "5", ")", "+", "2"] `shouldBe` True
    