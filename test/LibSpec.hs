module LibSpec (spec) where

import Lib
    ( isOperator,
      isOperand,
      operatorPrecedence,
      errorPrecedence,
      isOperatorLeftAssociative,
      errorLeftAssociativity,
      infixValidator,
      splitToList,
      removeSpaces, 
      infixToPostfix,
      popRemaining,
      popOperatorStack,
      popOperatorStackUpToParen,
      getFirstElem,
      evaluatePostfix,
      evaluatePostfix',
      evaluateExpression
      )
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
    describe "Validate function for isOperator" $ do
        it "returns True for isOperator 2" $ do
            (isOperator '2') `shouldBe` False 
        it "returns False for isOperator *" $ do
            (isOperator '*') `shouldBe` True 

    describe "Validate function for isOperand" $ do
        it "returns False for isOperand []" $ do
            (isOperand []) `shouldBe` False 
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

    describe "Validate function for errorPrecedence" $ do
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
        it "returns False for []" $ do
          infixValidator [] `shouldBe` False
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

    describe "Validate function for removeSpaces" $ do
        it "returns \"\" for \" \"" $ do
            removeSpaces " " `shouldBe` ""
        it "returns \"3+3342\" for \"3 + 334      2\"" $ do
            removeSpaces "3 + 334      2" `shouldBe` "3+3342"

    describe "Validate function for splitToList" $ do
        it "returns [] for []" $ do
            splitToList [] `shouldBe` []
        it "returns [] for \" \"" $ do
            splitToList "" `shouldBe` []
        it "returns [\"2\"] for \"   2   \"" $ do
            splitToList "   2    " `shouldBe` ["2"]
        it "returns [\"3\",\"+\",\"-\",\"3\",\"2\"] for \"     3 + -3 2  " $ do
            splitToList "        3 + -3 2   " `shouldBe` ["3","+","-3","2"] 
        it "returns [\"2\",\"3\"] for \"   2   3\"" $ do
            splitToList "   2    3" `shouldBe` ["2","3"] 
        it "returns [\"23\",\"+4\",\"-\",\"-\",\"-\",\"34\",\"-\",\"434\",\"-\",\"-\",\"34\",\"+\",\"2\"] for \"23 +4 ---34 -434 --34 + 2   \"" $ do
            splitToList "23 +4 ---34 -434 --34 + 2   " `shouldBe` ["23","+","4","-","-","-34","-434","-","-34","+","2"]
        it "returns [\"&\"] for \"&\"" $ do
            splitToList "&" `shouldBe` ["&"]
        it "returns [\"&\",\"4\"] for \"&4\"" $ do
            splitToList "&4" `shouldBe` ["&","4"]
    
    describe "Validate function for infixToPostfix" $ do
        it "returns [] for []" $ do
            infixToPostfix [] `shouldBe` []
        it "returns [\"3\", \"1\", \"+\"] for [\"3\", \"+\", \"1\"]" $ do
            infixToPostfix ["3", "+", "1"] `shouldBe` ["3", "1", "+"]
        it "returns [\"3\", \"1\", \"+\"] for [\"(\", \"3\", \"+\", \"1\", \")\"]" $ do
            infixToPostfix ["(", "3", "+", "1", ")"] `shouldBe` ["3", "1", "+"]
        it "returns [\"3\", \"1\", \"+\", \"4\", \"*\"] for [\"(\", \"3\", \"+\", \"1\", \")\", \"*\", \"4\"]" $ do
            infixToPostfix ["(", "3", "+", "1", ")", "*", "4"] `shouldBe` ["3","1","+","4","*"]
        it "returns [\"3\", \"1\", \"+\", \"4\", \"*\"] for [\"3\", \"+\", \"(\" \"1\", \"*\", \"4\", \")\"]" $ do
            infixToPostfix ["3", "*", "(", "1", "+", "4", ")"] `shouldBe` ["3","1","4","+","*"]
        it "returns [\"3\", \"1\", \"4\", \"/\", \"^\"] for [\"3\", \"^\", \"1\", \"/\", \"4\"]" $ do
            infixToPostfix ["3", "^", "1", "/", "4"] `shouldBe` ["3","1","^","4","/"]
        it "returns [\"3\", \"1\", \"*\", \"4\", \"^\"] for [\"(\", \"(\", \"3\", \"*\", \"1\", \")\", \"^\", \"4\", \")\"]" $ do
            infixToPostfix ["(", "(", "3", "*", "1", ")", "^", "4", ")"] `shouldBe` ["3","1","*","4","^"]
    
    describe "Validate function for popRemaining" $ do
        it "returns ([], [], []) for ([], [], [])" $ do
            popRemaining ([], [], []) `shouldBe` ([], [], [])
        it "returns ([\"+\"], [], [\"4\"]) for ([], [\"+\"], [\"4\"])" $ do
            popRemaining ([], ["+"], ["4"]) `shouldBe` (["+"], [], ["4"])
        it "returns ([\"3\", \"4\", \"+\", \"-\"], [], [\"4\"]) for ([\"3\", \"4\",], [\"+\", \"-\"], [\"4\"])" $ do
            popRemaining (["3", "4"], ["+", "-"], ["4"]) `shouldBe` (["3", "4", "+", "-"], [], ["4"])
        
    describe "Validate function for popOperatorStack" $ do
        it "returns ([], [\"+\"], []) for ([], [], []) \"+\"" $ do
            popOperatorStack ([], [], []) "+" `shouldBe` ([], ["+"], []) 
        it "returns ([\"3\"], [], [\"4\"]) for ([\"3\"], [\")\"], [\"4\"]) \"+\"" $ do
            popOperatorStack (["3"], [")"], ["4"]) "+" `shouldBe` (["3"], ["+", ")"], ["4"])
        it "returns ([\"3\", \"-\", \"+\"], [], [\"4\"]) for ([\"3\"], [\"-\", \"+\", \")\"], [\"4\"]) \"*\"" $ do
            popOperatorStack (["3"], ["-", "+", ")"], ["4"]) "*" `shouldBe` (["3"],["*","-","+",")"],["4"])
        it "returns ([\"3\"], [\"*\", \"/\", \")\"], [\"4\"]) for ([\"3\", \"*\", \"/\"], [\"+\", \")\"], [\"4\"]) \"+\"" $ do
            popOperatorStack (["3"], ["*", "/", ")"], ["4"]) "+" `shouldBe` (["3", "*", "/"],["+",")"],["4"])
    
    describe "Validate function for popOperatorStackUpToParen" $ do
        it "returns ([], [], []) for ([], [\"(\")], [])" $ do
            popOperatorStackUpToParen ([], ["("], []) `shouldBe` ([], [], [])
        it "returns ([], [], []) for ([], [], [])" $ do
            popOperatorStackUpToParen ([], [], []) `shouldBe` ([], [], []) 
        it "returns ([\"+\", \"-\"], [], []) for ([], [\"+\", \"-\", \"(\")], [])" $ do
            popOperatorStackUpToParen ([], ["+", "-", "("], []) `shouldBe` (["+", "-"], [], [])
    
    describe "Validate function for getFirstElem" $ do
        it "returns ([], [], []) for ([], [], [])" $ do
            getFirstElem ([], ["3"], []) `shouldBe` []
        it "returns [\"+\"] for ([\"+\"], [\"-\"], [\"/\"])" $ do
            getFirstElem (["+"], ["-"], ["/"]) `shouldBe` ["+"]

    describe "Validate function for evaluateExpression" $ do
        it "returns 6 for 3 * 2" $ do
            evaluateExpression 3 "*" 2 `shouldBe` 6 
        it "returns 9 for 18 / 2" $ do
            evaluateExpression 18 "/" 2 `shouldBe` 9
        it "returns 5 for 3 + 2" $ do
            evaluateExpression 3 "+" 2 `shouldBe` 5
        it "returns 1 for 3 - 2" $ do
            evaluateExpression 3 "-" 2 `shouldBe` 1
        it "returns 9 for 3 ^ 2" $ do
            evaluateExpression 3 "^" 2 `shouldBe` 9

    describe "Validate function for evaluatePostfix" $ do
        it "returns Nothing for []" $ do
            evaluatePostfix [] `shouldBe` Nothing
        it "returns Just 5 for [\"3\" \"2\" \"+\"]" $ do
            evaluatePostfix ["3", "2", "+"] `shouldBe` Just 5
        it "returns Just 20 for [\"3\" \"2\" \"+\", \"4\", \"*\"]" $ do
            evaluatePostfix ["3", "2", "+", "4", "*"] `shouldBe` Just 20
        it "returns Just -1.5 for [\"3\" \"2\" \"4\", \"-\", \"/\"]" $ do
            evaluatePostfix ["3", "2", "4", "-", "/"] `shouldBe` Just (-1.5)
        it "returns Just X for [\"3\" \"2\" \"4\", \"-\", \"*\", \"5\", \"^\"]" $ do
            evaluatePostfix ["3", "2", "+", "4", "*", "5", "^"] `shouldBe` Just 3200000 
        
