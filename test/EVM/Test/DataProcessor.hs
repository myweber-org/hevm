module DataProcessor where

import Data.Char (isDigit, toUpper)
import Data.List (intercalate)

-- Validate that a string contains only digits
validateDigits :: String -> Bool
validateDigits = all isDigit

-- Convert a string to uppercase
toUppercase :: String -> String
toUppercase = map toUpper

-- Process a list of strings: validate digits and convert to uppercase
processData :: [String] -> [String]
processData = map toUppercase . filter validateDigits

-- Format processed data as a comma-separated string
formatOutput :: [String] -> String
formatOutput = intercalate ", "

-- Main processing pipeline
processPipeline :: [String] -> String
processPipeline = formatOutput . processData

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
    let inputData = ["123", "abc", "456", "def", "789"]
    let processed = processPipeline inputData
    putStrLn $ "Input: " ++ show inputData
    putStrLn $ "Output: " ++ processedmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = all (\x -> x >= -100 && x <= 100) xs

main :: IO ()
main = do
    let inputData = [1, -2, 3, 0, 5, -8]
    if validateInput inputData
        then do
            let result = processData inputData
            putStrLn $ "Processed data: " ++ show result
        else putStrLn "Invalid input data detected"
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData = all (> 0)

main :: IO ()
main = do
    let inputData = [1, -2, 3, 0, 5, -8]
    let processed = processData inputData
    putStrLn $ "Original data: " ++ show inputData
    putStrLn $ "Processed data: " ++ show processed
    putStrLn $ "Data validation: " ++ show (validateData processed)