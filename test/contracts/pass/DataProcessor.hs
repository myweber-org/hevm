module DataProcessor where

import Data.Char (isDigit, toUpper)

-- Validate that a string contains only digits
validateDigits :: String -> Bool
validateDigits = all isDigit

-- Transform a string to uppercase
toUppercase :: String -> String
toUppercase = map toUpper

-- Process a list of strings: validate digits and transform to uppercase
processData :: [String] -> [Maybe String]
processData = map processItem
  where
    processItem str
      | validateDigits str = Just (toUppercase str)
      | otherwise = Nothing

-- Filter out Nothing values and extract Just values
extractValidData :: [Maybe String] -> [String]
extractValidData = foldr extractor []
  where
    extractor (Just x) acc = x : acc
    extractor Nothing acc = acc

-- Main processing pipeline
processPipeline :: [String] -> [String]
processPipeline = extractValidData . processDatamodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

import Data.List (tails)

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map avg $ filter (\window -> length window == n) $ tails xs
  where
    avg window = sum window / fromIntegral n

smoothData :: Fractional a => Int -> [a] -> [a]
smoothData windowSize dataPoints =
    let avg = movingAverage windowSize dataPoints
        padding = replicate (windowSize `div` 2) (head dataPoints)
    in padding ++ avg ++ padding

calculateTrend :: (Fractional a, Ord a) => [a] -> String
calculateTrend values
    | null values = "No data"
    | last values > head values = "Increasing trend"
    | last values < head values = "Decreasing trend"
    | otherwise = "Stable trend"

processDataset :: Fractional a => Int -> [a] -> ([a], String)
processDataset windowSize dataset =
    let smoothed = smoothData windowSize dataset
        trend = calculateTrend smoothed
    in (smoothed, trend)