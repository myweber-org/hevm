
module DataProcessor where

import Data.Char (isDigit, isAlpha, toUpper)
import Data.List (intercalate)

-- Validate if a string contains only digits
validateNumeric :: String -> Bool
validateNumeric = all isDigit

-- Validate if a string contains only alphabetic characters
validateAlpha :: String -> Bool
validateAlpha = all isAlpha

-- Normalize string by converting to uppercase and trimming spaces
normalizeString :: String -> String
normalizeString = map toUpper . trim
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- Process a list of strings: validate and normalize
processData :: [String] -> [String]
processData = map normalizeString . filter validateAlpha

-- Generate a report from processed data
generateReport :: [String] -> String
generateReport items =
  "Processed " ++ show (length items) ++ " items:\n" ++
  intercalate "\n" (zipWith (\i s -> show i ++ ". " ++ s) [1..] items)

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
  let rawData = ["hello", "world123", "  haskell  ", "123", "functional"]
      processed = processData rawData
  putStrLn $ generateReport processed