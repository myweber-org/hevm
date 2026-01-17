
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedList :: (Int -> Bool) -> (Int -> Int) -> [Int] -> Int
sumProcessedList predicate transformer = sum . filterAndTransform predicate transformer

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Even squares: " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of even squares: " ++ show (sumProcessedList even (\x -> x * x) numbers)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers
module DataProcessor where

import Data.Char (isDigit, toUpper)
import Data.List (intercalate)

-- Validates if a string contains only digits
validateDigits :: String -> Bool
validateDigits = all isDigit

-- Converts a string to uppercase
toUppercase :: String -> String
toUppercase = map toUpper

-- Removes all whitespace from a string
removeWhitespace :: String -> String
removeWhitespace = filter (not . (`elem` " \t\n"))

-- Formats a list of strings into a comma-separated string
formatList :: [String] -> String
formatList = intercalate ", "

-- Processes a list of strings by applying validation and transformations
processData :: [String] -> [String]
processData = map (toUppercase . removeWhitespace) . filter validateDigits

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
    let sampleData = ["123", "abc", "456 def", "789", "  "]
    let processed = processData sampleData
    putStrLn $ "Original: " ++ formatList sampleData
    putStrLn $ "Processed: " ++ formatList processed