module Lib
    ( divideSafe, isOperator, isOperand, 
      charToString, operatorPrecedence, 
      isOperatorLeftAssociative,
      infixValidator, popOperatorStackUpToParen,
      infixValidator', countBrackets, 
      errorPrecedence, errorLeftAssociativity,
      splitToList, removeSpaces, push,
      infixToPostfix, popRemaining,
      popOperatorStack, getFirstElem
    ) where

import Data.List (intersperse, groupBy)
import Data.Char (isSpace, ord, digitToInt, isNumber, isDigit, intToDigit)
import Data.Maybe (isJust, isNothing, fromMaybe)
-- Source: https://en.m.wikipedia.org/wifi/Shunting_yard/algorithm

isOperator :: Char -> Bool
isOperator x
 | x == '+' || x == '-' || x == '*' || x == '/' || x == '^' = True
 | otherwise = False

charToString :: Char -> String
charToString x = [x]

isOperand :: String -> Bool
isOperand "-" = False
isOperand (x:xs)
 | x == '-' = isOperand xs
 | x /= '-' = isDigit x && isOperand xs
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
 | isOperator (head x) && (isOperand (head xs) || head xs == "(") = infixValidator' xs
 | isOperand x && (isOperator (head (head xs)) || head xs == ")") = infixValidator' xs
 | x == ")"  && (isOperator (head (head xs))   || head xs == ")") = infixValidator' xs 
 | x == "("  && (isOperand (head xs)           || head xs == "(") = infixValidator' xs 
 | otherwise = False

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
splitToList []     = []
splitToList (x:xs) 
 | x == ' '  = splitToList xs 
 | isOperator x  = [x] : splitToList xs 
 | isOperand [x] = (x : fst (spanNum)) : splitToList (snd (spanNum))
 | otherwise = (x : fst (spanNum)) : fst (spanNum) : splitToList (snd (spanNum))-- (snd (spanNum))
 where spanNum = span (isNumber) xs
       spanOp  = span (isOperator) xs

removeSpaces :: String -> String
removeSpaces xs = filter (not . isSpace) xs

infixToPostfix :: [String] -> [String]
infixToPostfix [] = []
infixToPostfix xs = getFirstElem (infixToPostfix' ([], [], xs))

infixToPostfix' :: ([String], [String], [String]) -> ([String], [String], [String])
infixToPostfix' (xs, [], []) = (xs, [], [])
infixToPostfix' (xs, ys, []) = infixToPostfix' (popRemaining (xs, ys, []))
infixToPostfix' (xs, ys, z:zs) 
 | isOperand z = infixToPostfix' (xs ++ [z], ys, zs)
 | z == "(" = infixToPostfix' (xs, z:ys, zs)
 | isOperator (head z) = infixToPostfix' (popOperatorStack (xs, ys, zs) z)
 | z == ")" = infixToPostfix' (popOperatorStackUpToParen (xs, ys, zs))

getFirstElem :: ([a], [a], [a]) -> [a]
getFirstElem ([], _, _)  = []
getFirstElem (x, _, _) = x

popOperatorStack :: ([String], [String], [String]) -> String -> ([String], [String], [String])
popOperatorStack (xs, [],  zs) op = (xs, [op], zs) 
popOperatorStack (xs, y:ys, zs) op | isOperator (head y) && (operatorPrecedence (head y) > operatorPrecedence (head op)) = popOperatorStack (xs ++ [y], ys, zs) op
                                   | otherwise = (xs, op:y:ys, zs)

popOperatorStackUpToParen :: ([String], [String], [String]) -> ([String], [String], [String])
popOperatorStackUpToParen (xs, [], zs) = (xs, [], zs)
popOperatorStackUpToParen (xs, y:ys, zs) | y /= "(" = popOperatorStackUpToParen (xs ++ [y], ys, zs)
                                         | otherwise = (xs, ys, zs)

popRemaining :: ([String], [String], [String]) -> ([String], [String], [String])
popRemaining (xs, [], zs) = (xs, [], zs)
popRemaining (xs, y:ys, zs) = popRemaining (xs ++ [y], ys, zs)                                  