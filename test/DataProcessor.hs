module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform isEven square
  where
    isEven n = n `mod` 2 == 0
    square n = n * n

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)
module DataProcessor where

import Data.Char (isDigit, isAlpha, toUpper)
import Data.List (intercalate)

-- Validate if a string contains only digits
validateNumeric :: String -> Bool
validateNumeric = all isDigit

-- Validate if a string contains only alphabetic characters
validateAlpha :: String -> Bool
validateAlpha = all isAlpha

-- Normalize phone number by removing non-digit characters
normalizePhone :: String -> String
normalizePhone = filter isDigit

-- Format name by capitalizing first letter of each word
formatName :: String -> String
formatName = unwords . map capitalize . words
  where
    capitalize [] = []
    capitalize (x:xs) = toUpper x : map toLower xs
    toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

-- Process a list of strings with validation and transformation
processRecords :: [String] -> [Either String String]
processRecords = map processRecord
  where
    processRecord str
      | validateNumeric str = Right $ normalizePhone str
      | validateAlpha str = Right $ formatName str
      | otherwise = Left $ "Invalid record: " ++ str

-- Batch process records and separate valid/invalid results
batchProcess :: [String] -> ([String], [String])
batchProcess records =
  let results = processRecords records
      valid = [r | Right r <- results]
      invalid = [r | Left r <- results]
  in (valid, invalid)

-- Generate report from batch processing results
generateReport :: ([String], [String]) -> String
generateReport (valid, invalid) =
  "Processing Report:\n" ++
  "Valid records: " ++ show (length valid) ++ "\n" ++
  "Invalid records: " ++ show (length invalid) ++ "\n" ++
  "Valid data: " ++ intercalate ", " valid ++ "\n" ++
  "Errors: " ++ intercalate ", " invalid