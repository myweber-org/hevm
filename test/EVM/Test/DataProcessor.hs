module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | length xs < n = []
    | otherwise = avg (take n xs) : movingAverage n (tail xs)
  where
    avg ys = sum ys / fromIntegral (length ys)

-- Example usage with a helper function
exampleUsage :: IO ()
exampleUsage = do
    let dataSeries = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
    let windowSize = 3
    let result = movingAverage windowSize dataSeries
    putStrLn $ "Data series: " ++ show dataSeries
    putStrLn $ "Moving average (window=" ++ show windowSize ++ "): " ++ show result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processEvenSquares

main :: IO ()
main = do
    let sampleData = [1..10]
    putStrLn $ "Original data: " ++ show sampleData
    putStrLn $ "Processed data: " ++ show (processEvenSquares sampleData)
    putStrLn $ "Sum of processed data: " ++ show (sumProcessedData sampleData)
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
      | otherwise = "MIXED: " ++ str

-- Combine multiple strings with separator
combineWithSeparator :: String -> [String] -> String
combineWithSeparator sep = intercalate sep

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
  let testData = ["123-456-7890", "john", "test123", "doe"]
  let processed = processData testData
  putStrLn "Original data:"
  mapM_ putStrLn testData
  putStrLn "\nProcessed data:"
  mapM_ putStrLn processed
  putStrLn $ "\nCombined: " ++ combineWithSeparator " | " processed