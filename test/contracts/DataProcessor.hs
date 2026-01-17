
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