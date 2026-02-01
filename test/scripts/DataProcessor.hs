module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print resultmodule DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | length xs < n = []
    | otherwise = avg : movingAverage n (tail xs)
  where
    window = take n xs
    avg = sum window / fromIntegral n

-- Example usage with a helper function
demoMovingAverage :: IO ()
demoMovingAverage = do
    let dataSeries = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
    putStrLn "Original series:"
    print dataSeries
    putStrLn "\n3-period moving average:"
    print $ movingAverage 3 dataSeries
    putStrLn "\n5-period moving average:"
    print $ movingAverage 5 dataSeriesmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print resultmodule DataProcessor where

movingAverage :: (Fractional a) => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
  where
    windows m ys = take (length ys - m + 1) $ zipWith (++) (tails ys) (repeat [])
    average zs = sum zs / fromIntegral (length zs)

-- Helper function from Data.List
tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = (x:xs) : tails xs
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
  let testData = ["abc123", "456def", "valid", "789", "mixed123"]
      processed = processData testData
      validEntries = filterValidEntries processed
  
  putStrLn "Processed Data:"
  mapM_ print processed
  
  putStrLn "\nValid Entries:"
  mapM_ putStrLn validEntries