module Lib
    ( calculator, divideSafe
    ) where

calculator :: IO ()
calculator  = do
  putStrLn "This is my program. There are many like it but this one is mine."
  putStrLn ( "10 / 2 = " ++ show (divideSafe 10 2)) -- divideSafe(10, 2)
  putStrLn $ "10 / 0 = " ++ show (divideSafe 10 0)
  putStrLn $ "10 / 4 = " ++ show (divideSafe 10 4)


-- my first attempt - nasty errors await
-- divideSafe :: (Show a, Eq a, Fractional a) => a -> a -> a
-- divideSafe x y = x / y

-- my second attempt - adding Maybe a to support error return, but...
-- divideSafe :: (Show a, Eq a, Fractional a) => a -> a -> Maybe a
-- divideSafe x y = Just $ x / y


-- changing result to Maybe a, but something wrong?
-- divideSafe :: (Eq a, Fractional a) => a -> a -> Maybe a
-- divideSafe x y = Just $ x / y
-- divideSafe _ 0 = Nothing

-- getting there
--divideSafe :: (Eq a, Fractional a) => a -> a -> Maybe a
--divideSafe _ 0 = Nothing
--divideSafe x y = Just $ x / y

-- elaboration
-- TODO: i need to fix this seriously.
divideSafe :: (Ord a, Eq a, Fractional a) => a -> a -> Maybe a
divideSafe _ 0 = Nothing
divideSafe x y = if x > 100 then Nothing else Just $ x / y
