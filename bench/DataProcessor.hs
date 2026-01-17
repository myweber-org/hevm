
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

main :: IO ()
main = do
    let numbers = [-3, 1, 4, -1, 5, 9, -2, 6]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (\x -> x >= -100 && x <= 100) xs
                   then Just xs
                   else Nothing

main :: IO ()
main = do
    let sampleData = [-5, 2, 0, 8, -3, 10]
    case validateInput sampleData of
        Just validData -> do
            putStrLn $ "Original data: " ++ show validData
            let result = processData validData
            putStrLn $ "Processed data: " ++ show result
        Nothing -> putStrLn "Input validation failed: values must be between -100 and 100"
module DataProcessor where

import Data.Char (isDigit, isAlpha, toUpper)
import Data.List (intercalate)

-- Validate if a string contains only digits
validateNumeric :: String -> Bool
validateNumeric = all isDigit

-- Validate if a string contains only alphabetic characters
validateAlpha :: String -> Bool
validateAlpha = all isAlpha

-- Convert string to uppercase
toUppercase :: String -> String
toUppercase = map toUpper

-- Process a list of strings: validate and transform
processData :: [String] -> [String]
processData = map processItem
  where
    processItem str
      | validateNumeric str = "NUMERIC: " ++ str
      | validateAlpha str = "ALPHA: " ++ toUppercase str
      | otherwise = "MIXED: " ++ str

-- Format processed data as a single string
formatOutput :: [String] -> String
formatOutput items = intercalate "\n" items

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
  let testData = ["123", "abc", "a1b2c3", "HELLO", "456def"]
  let processed = processData testData
  putStrLn $ formatOutput processedmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let input = [1..10]
    let result = processData input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs
    | null xs = Nothing
    | any (< -100) xs = Nothing
    | any (> 100) xs = Nothing
    | otherwise = Just xs
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessed :: (Int -> Int) -> [Int] -> Int
sumProcessed processor = sum . map processor

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Even squares: " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of doubled values: " ++ show (sumProcessed (*2) numbers)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processNumbers input
    print result