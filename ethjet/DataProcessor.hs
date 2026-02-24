
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
--   putStrLn $ fullPipeline sampleDatamodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Maybe [Int]
validateInput xs
    | null xs = Nothing
    | any (< -100) xs = Nothing
    | any (> 100) xs = Nothing
    | otherwise = Just xsmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = all (\x -> x >= -100 && x <= 100) xs

main :: IO ()
main = do
    let sampleData = [1, -2, 3, 0, 5, -10]
    if validateInput sampleData
        then print $ processData sampleData
        else putStrLn "Invalid input data"module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData xs = all (> 0) xs && length xs > 3

combineResults :: [Int] -> [Int] -> [Int]
combineResults xs ys = zipWith (+) (processData xs) (processData ys)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, 0, 5, -7]
    let result = processNumbers input
    print result
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
  let testData = ["abc123", "hello", "12345", "test456"]
      processed = processData testData
      validEntries = filterValidEntries processed
  
  putStrLn "Original data:"
  mapM_ print testData
  
  putStrLn "\nProcessed data (original, isValid, transformed):"
  mapM_ print processed
  
  putStrLn "\nValid transformed entries:"
  mapM_ putStrLn validEntriesmodule DataProcessor where

processData :: [Int] -> [Int]
processData xs = map (^2) (filter even xs)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (^2)

calculateStats :: [Int] -> (Int, Int, Double)
calculateStats xs = (minimum xs, maximum xs, average)
  where
    average = fromIntegral (sum xs) / fromIntegral (length xs)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

processData :: [Int] -> Maybe (Int, Int, Double)
processData xs = do
    first <- safeHead xs
    let processed = processEvenSquares xs
    if null processed
        then Nothing
        else Just (first, length processed, sum processed)