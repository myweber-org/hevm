module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData xs = all (> 0) xs && length xs > 3

combineResults :: [Int] -> [Int] -> [Int]
combineResults xs ys = zipWith (+) (processData xs) (processData ys)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let input = [1..10]
    let result = processData input
    putStrLn $ "Original list: " ++ show input
    putStrLn $ "Processed list: " ++ show result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles xs = sum $ processNumbers xsmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, 0, 5, -8]
    let result = processData input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
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
toUpperCase :: String -> String
toUpperCase = map toUpper

-- Normalize phone number by removing non-digit characters
normalizePhone :: String -> String
normalizePhone = filter isDigit

-- Format name as "Last, First"
formatName :: String -> String -> String
formatName first last = last ++ ", " ++ first

-- Process a list of strings with validation and transformation
processData :: [String] -> [String]
processData = map processItem
  where
    processItem str
      | validateNumeric str = "NUMERIC: " ++ normalizePhone str
      | validateAlpha str   = "ALPHA: " ++ toUpperCase str
      | otherwise           = "MIXED: " ++ str

-- Calculate statistics about processed data
calculateStats :: [String] -> (Int, Int, Int)
calculateStats dataList = (numericCount, alphaCount, mixedCount)
  where
    numericCount = length $ filter (isPrefixOf "NUMERIC") dataList
    alphaCount = length $ filter (isPrefixOf "ALPHA") dataList
    mixedCount = length $ filter (isPrefixOf "MIXED") dataList
    isPrefixOf prefix str = prefix `elem` words str

-- Generate report from processed data
generateReport :: [String] -> String
generateReport dataList = 
    let stats = calculateStats dataList
        (numCount, alphaCount, mixedCount) = stats
        total = numCount + alphaCount + mixedCount
    in intercalate "\n" $
        ["Data Processing Report", "======================"] ++
        ["Total items: " ++ show total] ++
        ["Numeric items: " ++ show numCount] ++
        ["Alpha items: " ++ show alphaCount] ++
        ["Mixed items: " ++ show mixedCount] ++
        ["", "Processed Data:"] ++
        map ("  - " ++) dataList

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
    let testData = ["123-456-7890", "John", "Doe123", "Alice", "555-1234"]
    let processed = processData testData
    putStrLn $ generateReport processedmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    let result = processNumbers numbers
    print result