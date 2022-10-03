module Lib
    ( divideSafe, isOperator, isOperand, 
      charToString, operatorPrecedence, 
      isOperatorLeftAssociative,
      infixValidator,
      infixValidator', countBrackets, 
      errorPrecedence, errorLeftAssociativity,
      splitToList, removeSpaces, push 
    ) where

import Data.List (intersperse, groupBy)
import Data.Char (isSpace, ord, digitToInt, isNumber, isDigit, intToDigit)
import Data.Maybe (isJust, isNothing, fromMaybe)
-- Source: https://en.m.wikipedia.org/wifi/Shunting_yard/algorithm

isOperator :: Char -> Bool
isOperator x = x `elem` "+-*/^"

charToString :: Char -> String
charToString x = [x]

isOperand :: String -> Bool
isOperand "-" = False
isOperand (x:xs) = (isDigit x || x == '-') && isOperand xs
isOperand [] = True

divideSafe :: (Eq a, Fractional a) => a -> a -> Maybe a
divideSafe _ 0 = Nothing
divideSafe x y = Just $ x / y

operatorPrecedence :: Char -> Maybe Int 
operatorPrecedence x
 | x == '^'             = Just 4
 | x == '*' || x == '/' = Just 3
 | x == '+' || x == '-' = Just 2
 | otherwise            = Nothing 

errorPrecedence :: Maybe Int -> String
errorPrecedence x =
    case x of
        Just _  -> "It has a precedence"
        Nothing -> "Error, does not have a precedence" 

isOperatorLeftAssociative :: Char -> Maybe Bool 
isOperatorLeftAssociative x
 | x == '^'     = Just False
 | isOperator x = Just True
 | otherwise    = Nothing 

errorLeftAssociativity :: Maybe Bool -> String
errorLeftAssociativity x =
    case x of
        Just _  -> "It has an associativity"
        Nothing -> "Error, does not have associativity" 

infixValidator :: [String] -> Bool
infixValidator [] = False
infixValidator xs = infixValidator' xs && countBrackets xs 0 0 

infixValidator' :: [String] -> Bool
infixValidator' (x:[]) = True
infixValidator' (x:xs) 
 | isOperator (head x) && (firstOperandElem || head xs == "(") = infixValidator' xs
 | isOperand x        && (firstOperatorElem || head xs == ")") = infixValidator' xs
 | x == ")"           && (firstOperatorElem || head xs == ")") = infixValidator' xs 
 | x == "("           && (firstOperandElem  || head xs == "(") = infixValidator' xs 
 | otherwise = False
 where firstOperandElem  = isOperand . head  $ xs 
       firstOperatorElem = isOperator . head . head $ xs

countBrackets :: [String] -> Int -> Int -> Bool
countBrackets [] open close = open == close 
countBrackets (x:xs) open close 
 | x == "("  = countBrackets xs (open+1) close
 | x == ")"  = countBrackets xs open (close+1)
 | otherwise = countBrackets xs open close 

-- Stack implementations
push :: a -> [a] -> [a]
push n xs = reverse $ n : reverse xs

-- pop == init, init will cause error if popping empty list

splitToList :: String -> [String]
splitToList [] = []
splitToList (x:xs) 
 | x == ' '             = splitToList xs
 | not (isOperand [x])  = [x] : splitToList xs 
 | otherwise            = (x : fst (spanNum)) : splitToList (snd (spanNum))
 where spanNum = span (isNumber) xs

removeSpaces :: String -> String
removeSpaces xs = filter (not . isSpace) xs
