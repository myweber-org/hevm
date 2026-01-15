
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
      | validateAlpha str   = "ALPHA: " ++ toUppercase str
      | otherwise          = "MIXED: " ++ str

-- Combine multiple strings with a separator
combineWithSeparator :: String -> [String] -> String
combineWithSeparator sep = intercalate sep . filter (not . null)

-- Safe head function with default value
safeHead :: a -> [a] -> a
safeHead def [] = def
safeHead _ (x:_) = x

-- Calculate average of a list of numbers
average :: [Double] -> Double
average [] = 0
average xs = sum xs / fromIntegral (length xs)