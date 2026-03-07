
module DataProcessor where

import Data.Char (isDigit, isAlpha, toUpper)
import Data.List (intercalate)

-- Validate if string contains only digits
validateNumeric :: String -> Bool
validateNumeric = all isDigit

-- Validate if string contains only alphabetic characters
validateAlpha :: String -> Bool
validateAlpha = all isAlpha

-- Convert string to standardized uppercase format
standardizeText :: String -> String
standardizeText = map toUpper

-- Process a list of strings with validation and transformation
processData :: [String] -> (Int, Int, [String])
processData items = 
    let numericCount = length $ filter validateNumeric items
        alphaCount = length $ filter validateAlpha items
        processed = map standardizeText items
    in (numericCount, alphaCount, processed)

-- Generate a summary report from processed data
generateReport :: (Int, Int, [String]) -> String
generateReport (numCount, alphaCount, items) =
    "Data Processing Report\n" ++
    "=====================\n" ++
    "Numeric entries: " ++ show numCount ++ "\n" ++
    "Alphabetic entries: " ++ show alphaCount ++ "\n" ++
    "Total entries: " ++ show (length items) ++ "\n" ++
    "Processed items: " ++ intercalate ", " items

-- Main processing pipeline
runDataProcessor :: [String] -> String
runDataProcessor input = 
    let processed = processData input
    in generateReport processed
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 10) (* 2)

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> 0) xs then Just xs else Nothing

main :: IO ()
main = do
    let sampleData = [5, 12, 8, 20, 3, 15]
    case validateInput sampleData of
        Just validData -> do
            putStrLn "Original data:"
            print validData
            putStrLn "Processed data (values > 10 doubled):"
            print $ processData validData
        Nothing -> putStrLn "Invalid input: all values must be positive"module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData