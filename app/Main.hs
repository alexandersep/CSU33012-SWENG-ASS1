module Main where

import Lib
import System.IO

main :: IO ()
main = do
    putStrLn "Haskell Infix Calculator, Note: Input without Quotes e.g. 2 + 3 instead of \"2 + 3\""
    putStr "Please input an infix expression: "
    hFlush stdout -- flush standard output explicitly in order to run getLine in the same line as putStr
    infixExpr <- getLine
    let splitInfixExrp = splitToList $ infixExpr
    let isInfixValid = infixValidator $ splitInfixExrp 
    let infixCalculation = infixToPostfix $ splitInfixExrp 
    if isInfixValid == True 
        then do let expr = splitToList $ infixExpr  
                putStr "The answer is: "
                putStrLn $ show infixCalculation 
        else do putStrLn "Invalid"
                putStrLn "The answer cannot be calculated" 
    return ()
