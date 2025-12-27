
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumPositiveDoubled :: [Int] -> Int
sumPositiveDoubled = sum . processDatamodule DataProcessor where

import Data.Char (isDigit, isLetter, toUpper)

-- Validate if a string contains only alphanumeric characters
isAlphanumeric :: String -> Bool
isAlphanumeric = all (\c -> isDigit c || isLetter c)

-- Normalize a string by converting to uppercase and trimming whitespace
normalizeString :: String -> String
normalizeString = map toUpper . trim
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- Process a list of strings, keeping only valid alphanumeric ones
processData :: [String] -> [String]
processData = filter isAlphanumeric . map normalizeString

-- Calculate statistics on processed data
dataStats :: [String] -> (Int, Int, Double)
dataStats xs = (total, avgLength, fromIntegral total / fromIntegral (length xs))
  where
    total = length xs
    avgLength = if null xs then 0 else sum (map length xs) `div` total