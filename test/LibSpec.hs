module LibSpec (spec) where

import           Lib
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
    describe "Operand Function" $ do

        it "returns Just 0 for 0/3" $ do
            (divideSafe 0 3)  `shouldBe` Just 0
        it "returns Just 5 for 10/2" $ do
            (divideSafe 10 2) `shouldBe` Just 5
        it "returns Nothing for 10/0" $ do
            (divideSafe 10 0) `shouldBe` Nothing

        it "returns True for isOperator 2" $ do
            (isOperator '2') `shouldBe` False 
        it "returns False for isOperator *" $ do
            (isOperator '*') `shouldBe` True 

        it "returns True for isOperand 2" $ do
            (isOperand "2") `shouldBe` True
        it "returns False for isOperand *" $ do
            (isOperand "*") `shouldBe` False
        it "returns True for isOperand 100" $ do
            (isOperand "100") `shouldBe` True

        it "returns 1 for charToString 1" $ do
            (charToString '1') `shouldBe` "1" 

        it "returns Nothing for operatorPrecedence a" $ do
            (operatorPrecedence 'a') `shouldBe` Nothing 
        it "returns Just 4 for operatorPrecedence ^" $ do
            (operatorPrecedence '^') `shouldBe` Just 4 
        it "returns Just 3 for operatorPrecedence *" $ do
            (operatorPrecedence '*') `shouldBe` Just 3 
        it "returns Just 3 for operatorPrecedence /" $ do
            (operatorPrecedence '/') `shouldBe` Just 3 
        it "returns Just 2 for operatorPrecedence +" $ do
            (operatorPrecedence '+') `shouldBe` Just 2
        it "returns Just 2 for operatorPrecedence -" $ do
            (operatorPrecedence '-') `shouldBe` Just 2 

        it "returns It has a precedence for operatorPrecedence Just 4" $ do
            (errorPrecedence $ Just 4) `shouldBe` "It has a precedence"
        it "returns Error, does not have associativity for operatorPrecedence Nothing" $ do
            (errorPrecedence $ Nothing) `shouldBe` "Error, does not have a precedence" 

        it "returns Nothing for isOperatorLeftAssociative a" $ do
            (isOperatorLeftAssociative 'a') `shouldBe` Nothing
        it "returns Just False for isOperatorLeftAssociative ^" $ do
            (isOperatorLeftAssociative '^') `shouldBe` Just False 
        it "returns Just True for isOperatorLeftAssociative +" $ do
            (isOperatorLeftAssociative '+') `shouldBe` Just True
        it "returns Just True for isOperatorLeftAssociative -" $ do
            (isOperatorLeftAssociative '-') `shouldBe` Just True
        it "returns Just True for isOperatorLeftAssociative *" $ do
            (isOperatorLeftAssociative '*') `shouldBe` Just True
        it "returns Just True for isOperatorLeftAssociative /" $ do
            (isOperatorLeftAssociative '/') `shouldBe` Just True

        it "returns Error, does not have associativity for errorLeftAssociativity Just True" $ do
            (errorLeftAssociativity $ Just True) `shouldBe` "It has an associativity" 
        it "returns Error, does not have associativity for errorLeftAssociativity Nothing" $ do
            (errorLeftAssociativity $ Nothing) `shouldBe` "Error, does not have associativity" 
        
--        it 
--            (operatorPrecedence '^') `shouldBe`  

    --     it "returns Just 5 for 10/2" $ do
    --       (divideSafe 10 2) `shouldBe` Just 5

    --     it "returns Nothing for x/0" $ do
    --       (divideSafe 10 0) `shouldBe` Nothing

    --     -- some time after development starts we decide that we want
    --     -- to return nothing if x > 100. This might arise because
    --     -- stakeholders decide its a new feature requirement.
    --     --  We add test cases for it.
    --     -- Two that come to mind are to test that out code (that we
    --     -- have not yet rewritten, will still work with x > 100 and y = 0.
    --     -- A second case is x > 100 and y > 0 - the nortmal case.
    --     it "should return nothing for x > 100, y == 0" $ do
    --       (divideSafe 101 0) `shouldBe` Nothing

    --     it "should return nothing for x > 100, y == 10" $ do
    --       (divideSafe 101 10) `shouldBe` Nothing
