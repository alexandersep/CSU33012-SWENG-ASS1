module Lib
    ( calculator, divideSafe, isOperator, isOperand, 
      charToString, operatorPrecedence, 
      isOperatorLeftAssociative, 
      errorPrecedence, errorLeftAssociativity
    ) where

import Data.Char (isDigit, intToDigit)
import Data.Maybe (isJust, isNothing, fromMaybe)
-- Source: https://en.m.wikipedia.org/wifi/Shunting_yard/algorithm

calculator :: IO ()
calculator  = do
  putStrLn "This is my program. There are many like it but this one is mine."
  putStrLn $ "0 / 3 = " ++ show (divideSafe 0 3)
  putStrLn $ "10 / 2 = " ++ show (divideSafe 10 2)
  putStrLn $ "10 / 0 = " ++ show (divideSafe 10 0)
  putStrLn $ "isOperator '2'  = " ++ show (isOperator '2')
  putStrLn $ "isOperator '*'  = " ++ show (isOperator '*')
  putStrLn $ "isOperand 2  = " ++ show (isOperand "2")
  putStrLn $ "isOperand *  = " ++ show (isOperand "*")
  putStrLn $ "isOperand 100  = " ++ show (isOperand "100")
  putStrLn $ "charToString a = " ++ show (charToString 'a')
  putStrLn $ "operatorPrecedence a = " ++ show (operatorPrecedence 'a')
  putStrLn $ "operatorPrecedence ^ = " ++ show (operatorPrecedence '^')
  putStrLn $ "operatorPrecedence * = " ++ show (operatorPrecedence '*')
  putStrLn $ "operatorPrecedence / = " ++ show (operatorPrecedence '/')
  putStrLn $ "operatorPrecedence + = " ++ show (operatorPrecedence '+')
  putStrLn $ "operatorPrecedence - = " ++ show (operatorPrecedence '-')
  putStrLn $ "errorPrecedence Just 4 = " ++ show (errorPrecedence $ Just 4)
  putStrLn $ "errorPrecedence Nothing = " ++ show (errorPrecedence Nothing)
  putStrLn $ "isOperatorLeftAssociative a = " ++ show (isOperatorLeftAssociative 'a')
  putStrLn $ "isOperatorLeftAssociative ^ = " ++ show (isOperatorLeftAssociative '^')
  putStrLn $ "isOperatorLeftAssociative + = " ++ show (isOperatorLeftAssociative '+')
  putStrLn $ "isOperatorLeftAssociative - = " ++ show (isOperatorLeftAssociative '-')
  putStrLn $ "isOperatorLeftAssociative * = " ++ show (isOperatorLeftAssociative '*')
  putStrLn $ "isOperatorLeftAssociative / = " ++ show (isOperatorLeftAssociative '/')
  putStrLn $ "errorLeftAssociativity Just True = " ++ show (errorLeftAssociativity $ Just True)
  putStrLn $ "errorLeftAssociativity Nothing = " ++ show (errorLeftAssociativity Nothing)

isOperator :: Char -> Bool
isOperator x
 | x == '+' || x == '-' || x == '*' || x == '/' || x == '^' = True
 | otherwise = False

charToString :: Char -> String
charToString x = [x]

isOperand :: String -> Bool
isOperand (x:xs) = isDigit x && isOperand xs 
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
