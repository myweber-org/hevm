
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