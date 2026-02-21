
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
      | validateAlpha str = "ALPHA: " ++ toUppercase str
      | otherwise = "INVALID: " ++ str

-- Combine multiple processing steps
fullPipeline :: [(String, String)] -> String
fullPipeline names = 
  let formatted = map (\(f, l) -> formatName f l) names
      processed = processData formatted
  in intercalate "\n" processed

-- Example usage in a separate main function would be:
-- main :: IO ()
-- main = do
--   let sampleData = [("John", "Doe"), ("123-456-7890", ""), ("test123", "")]
--   putStrLn $ fullPipeline sampleData