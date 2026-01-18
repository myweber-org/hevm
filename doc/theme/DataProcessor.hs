module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Bool
validateInput = all (\x -> x >= -100 && x <= 100)

safeProcess :: [Int] -> Maybe Int
safeProcess xs
    | validateInput xs = Just (sumProcessedData xs)
    | otherwise = Nothingmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -100) xs then Just xs else Nothing
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = 
    filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just xmodule DataProcessor where

import Data.Char (toUpper, isDigit)
import Data.List (intercalate)

-- Validate that a string contains only alphabetic characters
validateAlpha :: String -> Bool
validateAlpha = all (`elem` ['A'..'Z'] ++ ['a'..'z'])

-- Convert a string to uppercase
toUpperCase :: String -> String
toUpperCase = map toUpper

-- Extract numeric characters from a string
extractNumbers :: String -> String
extractNumbers = filter isDigit

-- Process a list of strings: validate, convert to uppercase, and extract numbers
processData :: [String] -> [(String, String, String)]
processData = map processSingle
  where
    processSingle str =
      let valid = validateAlpha str
          upper = toUpperCase str
          numbers = extractNumbers str
      in (show valid, upper, numbers)

-- Format processed data as a CSV string
formatAsCSV :: [(String, String, String)] -> String
formatAsCSV records =
  "IsValid,UpperCase,Numbers\n" ++
  intercalate "\n" (map (\(v, u, n) -> v ++ "," ++ u ++ "," ++ n) records)

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
  let testData = ["Hello123", "World456", "Test789"]
      processed = processData testData
      csvOutput = formatAsCSV processed
  putStrLn "Processed Data:"
  print processed
  putStrLn "\nCSV Output:"
  putStrLn csvOutput