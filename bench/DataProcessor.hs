module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-5, 3, 0, 8, -2, 10]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

main :: IO ()
main = do
    let numbers = [-3, 2, 0, 7, -1, 4]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)
    putStrLn $ "First element: " ++ show (safeHead numbers)module DataProcessor where

import Data.Char (toUpper)

-- Validate that a string is not empty and contains only letters
validateName :: String -> Maybe String
validateName "" = Nothing
validateName name
    | all isLetter name = Just name
    | otherwise = Nothing
  where
    isLetter c = c `elem` ['a'..'z'] ++ ['A'..'Z']

-- Convert a string to uppercase
toUppercase :: String -> String
toUppercase = map toUpper

-- Process a name: validate then transform
processName :: String -> Maybe String
processName name = do
    validated <- validateName name
    return $ toUppercase validated

-- Calculate average of a list of numbers
calculateAverage :: [Double] -> Maybe Double
calculateAverage [] = Nothing
calculateAverage xs = Just (sum xs / fromIntegral (length xs))

-- Filter even numbers from a list
filterEven :: [Int] -> [Int]
filterEven = filter even

-- Main processing pipeline example
processData :: [String] -> [Maybe String]
processData = map processNamemodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, 0, 5, -7]
    let result = processNumbers input
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbers