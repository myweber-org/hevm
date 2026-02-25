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
    putStrLn $ "Output: " ++ processed