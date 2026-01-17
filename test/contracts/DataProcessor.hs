
module DataProcessor where

import Data.Char (isDigit, toUpper)
import Data.List (intercalate)

-- Validate if a string contains only digits
validateDigits :: String -> Bool
validateDigits = all isDigit

-- Transform a string to uppercase
toUppercase :: String -> String
toUppercase = map toUpper

-- Process a list of strings: validate and transform valid entries
processData :: [String] -> [String]
processData = map toUppercase . filter validateDigits

-- Format processed data as a comma-separated string
formatOutput :: [String] -> String
formatOutput = intercalate ", "

-- Main processing pipeline
pipeline :: [String] -> String
pipeline = formatOutput . processData
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

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