module LibSpec (spec) where

import           Lib
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
    describe "Validate function for divideSafe" $ do
        it "returns Just 0 for 0/3" $ do
            (divideSafe 0 3)  `shouldBe` Just 0
        it "returns Just 5 for 10/2" $ do
            (divideSafe 10 2) `shouldBe` Just 5
        it "returns Nothing for 10/0" $ do
            (divideSafe 10 0) `shouldBe` Nothing

    describe "Validate function for isOperator" $ do
        it "returns True for isOperator 2" $ do
            (isOperator '2') `shouldBe` False 
        it "returns False for isOperator *" $ do
            (isOperator '*') `shouldBe` True 

    describe "Validate function for isOperand" $ do
        it "returns True for isOperand 2" $ do
            (isOperand "2") `shouldBe` True
        it "returns False for isOperand *" $ do
            (isOperand "*") `shouldBe` False
        it "returns True for isOperand 100" $ do
            (isOperand "100") `shouldBe` True
        it "returns True for isOperand -1" $ do
            (isOperand "-1") `shouldBe` True
        it "returns True for isOperand ------1" $ do
            (isOperand "-----1") `shouldBe` True
        it "returns True for isOperand - ------1" $ do
            (isOperand "- -----1") `shouldBe` False

    describe "Validate function for charToString" $ do
        it "returns 1 for charToString 1" $ do
            (charToString '1') `shouldBe` "1" 

    describe "Validate function for operatorPrecedence" $ do
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

    describe "Validate function for isOperatorLeftAssociative" $ do
        it "returns It has a precedence for operatorPrecedence Just 4" $ do
            (errorPrecedence $ Just 4) `shouldBe` "It has a precedence"
        it "returns Error, does not have associativity for operatorPrecedence Nothing" $ do
            (errorPrecedence $ Nothing) `shouldBe` "Error, does not have a precedence" 

    describe "Validate function for isOperatorLeftAssociative" $ do
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

    describe "Validate function for errorLeftAssociativity" $ do
        it "returns Error, does not have associativity for errorLeftAssociativity Just True" $ do
            (errorLeftAssociativity $ Just True) `shouldBe` "It has an associativity" 
        it "returns Error, does not have associativity for errorLeftAssociativity Nothing" $ do
            (errorLeftAssociativity $ Nothing) `shouldBe` "Error, does not have associativity" 

    describe "Validate function for Infix Expressions" $ do
        it "returns False for (30 + 5)2" $ do
          infixValidator ["(", "30", "+", "5", ")", "2"] `shouldBe` False
        
        it "returns True for (30 + 5) + 2" $ do
          infixValidator ["(", "30", "+", "5", ")", "+", "2"] `shouldBe` True
        
        it "returns invalid for (30 + 5) + )" $ do
          infixValidator ["(", "30", "+", "5", ")", "+", ")"] `shouldBe` False
        
        it "returns False for (30 + 5) + 2)" $ do
          infixValidator ["(", "30", "+", "5", ")", "+", "2", ")"] `shouldBe` False

        it "returns True for (1 + -1) + 2" $ do
          infixValidator ["(", "1", "+", "-1", ")", "+", "2"] `shouldBe` True

        it "returns True for (--1 + --2) + -2" $ do
          infixValidator ["(", "--1", "+", "--2", ")", "+", "-2"] `shouldBe` True

        it "returns False for () 1 + *) + 2" $ do
          infixValidator ["(", ")", "1", "+", "*", ")", "+", "2"] `shouldBe` False

        it "returns False for (* + /) + 2" $ do
          infixValidator ["(", "*", "+", "/", ")", "+", "2"] `shouldBe` False

        it "returns False for (- ------1 + 3)" $ do
          infixValidator ["(", "-", "------1", "+", "3", ")"] `shouldBe` False

