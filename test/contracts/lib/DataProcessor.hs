
module DataProcessor where

import Data.Char (isDigit, toUpper)
import Data.List (intercalate)

-- Validate if a string contains only digits
validateDigits :: String -> Bool
validateDigits = all isDigit

-- Transform a string to uppercase
toUppercase :: String -> String
toUppercase = map toUpper

-- Process a list of strings: validate digits and transform to uppercase
processData :: [String] -> [String]
processData = map toUppercase . filter validateDigits

-- Format processed data into a single string
formatOutput :: [String] -> String
formatOutput items = "Processed items: " ++ intercalate ", " items

-- Main processing pipeline
processPipeline :: [String] -> String
processPipeline = formatOutput . processData