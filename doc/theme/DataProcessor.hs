module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData xs = all (> 0) xs && length xs > 3

combineResults :: [Int] -> [Int] -> [Int]
combineResults xs ys = zipWith (+) (processData xs) (processData ys)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processData

main :: IO ()
main = do
    let testData = [-3, 2, 0, 5, -1, 8]
    putStrLn $ "Original data: " ++ show testData
    putStrLn $ "Processed data: " ++ show (processData testData)
    putStrLn $ "Sum of positive doubles: " ++ show (sumPositiveDoubles testData)
module DataProcessor where

import Data.Char (isDigit, isAlpha, toUpper)

-- Validate if a string contains only digits
validateNumeric :: String -> Bool
validateNumeric = all isDigit

-- Validate if a string contains only alphabetic characters
validateAlpha :: String -> Bool
validateAlpha = all isAlpha

-- Convert string to uppercase
toUppercase :: String -> String
toUppercase = map toUpper

-- Process a list of strings: validate and transform
processData :: [String] -> [String]
processData = map processItem
  where
    processItem item
      | validateNumeric item = "NUMERIC: " ++ item
      | validateAlpha item = "ALPHA: " ++ toUppercase item
      | otherwise = "INVALID: " ++ item

-- Filter valid numeric entries
filterNumeric :: [String] -> [String]
filterNumeric = filter validateNumeric

-- Filter valid alphabetic entries
filterAlpha :: [String] -> [String]
filterAlpha = filter validateAlpha

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
  let testData = ["123", "abc", "456def", "XYZ", "789"]
  putStrLn "Original data:"
  print testData
  putStrLn "\nProcessed data:"
  print $ processData testData
  putStrLn "\nNumeric entries:"
  print $ filterNumeric testData
  putStrLn "\nAlphabetic entries:"
  print $ filterAlpha testData
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedList :: [Int] -> Int
sumProcessedList = sum . processEvenSquares

main :: IO ()
main = do
    let sampleData = [1..10]
    putStrLn $ "Original list: " ++ show sampleData
    putStrLn $ "Processed list: " ++ show (processEvenSquares sampleData)
    putStrLn $ "Sum of processed list: " ++ show (sumProcessedList sampleData)