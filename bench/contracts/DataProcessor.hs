
module DataProcessor where

import Data.Char (isDigit, isAlpha, toUpper)

-- Validate if a string contains only digits
validateNumeric :: String -> Bool
validateNumeric = all isDigit

-- Validate if a string contains only alphabetic characters
validateAlpha :: String -> Bool
validateAlpha = all isAlpha

-- Transform string to uppercase
transformToUpper :: String -> String
transformToUpper = map toUpper

-- Process a list of strings with validation and transformation
processData :: [String] -> [(String, Bool, String)]
processData = map processSingle
  where
    processSingle str =
      let numericValid = validateNumeric str
          alphaValid = validateAlpha str
          transformed = transformToUpper str
      in (str, numericValid && alphaValid, transformed)

-- Filter valid entries from processed data
filterValidEntries :: [(String, Bool, String)] -> [String]
filterValidEntries = map (\(_, _, transformed) -> transformed) . filter (\(_, valid, _) -> valid)

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
  let testData = ["abc123", "test", "12345", "HELLO", "mixed123"]
  let processed = processData testData
  let validEntries = filterValidEntries processed
  
  putStrLn "Original data:"
  mapM_ print testData
  
  putStrLn "\nProcessed data (original, isValid, transformed):"
  mapM_ print processed
  
  putStrLn "\nValid transformed entries:"
  mapM_ putStrLn validEntriesmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x