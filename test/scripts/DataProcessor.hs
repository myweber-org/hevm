module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    let result = processNumbers numbers
    print result
module DataProcessor where

import Data.Char (isDigit, isSpace)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)

-- | Validates if a string contains only digits
validateDigits :: String -> Bool
validateDigits = all isDigit

-- | Safely parses an integer from a string
safeParseInt :: String -> Maybe Int
safeParseInt str
    | validateDigits str = Just (read str)
    | otherwise = Nothing

-- | Trims leading and trailing whitespace
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

-- | Transforms a list of strings into a comma-separated string
joinWithCommas :: [String] -> String
joinWithCommas = intercalate ", "

-- | Processes a list of string inputs into valid integers
processNumbers :: [String] -> [Int]
processNumbers = catMaybes . map safeParseInt . map trim

-- | Calculates statistics from a list of numbers
calculateStats :: [Int] -> (Int, Int, Double)
calculateStats [] = (0, 0, 0.0)
calculateStats nums =
    let total = sum nums
        count = length nums
        average = fromIntegral total / fromIntegral count
    in (total, count, average)

-- | Main processing pipeline
processData :: [String] -> String
processData inputs =
    let cleaned = map trim inputs
        numbers = processNumbers cleaned
        (total, count, avg) = calculateStats numbers
        validCount = length numbers
        invalidCount = length inputs - validCount
    in unlines
        [ "Processing Results:"
        , "Valid numbers: " ++ show validCount
        , "Invalid inputs: " ++ show invalidCount
        , "Total sum: " ++ show total
        , "Average: " ++ show avg
        , "All valid numbers: " ++ show numbers
        ]
module DataProcessor where

import Data.Char (isDigit, isAlpha, toUpper)

-- Validate that a string contains only digits
validateNumeric :: String -> Bool
validateNumeric = all isDigit

-- Validate that a string contains only alphabetic characters
validateAlpha :: String -> Bool
validateAlpha = all isAlpha

-- Normalize string by converting to uppercase and trimming whitespace
normalizeString :: String -> String
normalizeString = map toUpper . trim
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- Process a list of strings: validate, normalize, and filter invalid entries
processData :: [String] -> [String]
processData = map normalizeString . filter validateAlpha

-- Calculate statistics on numeric data
calculateStats :: [Int] -> (Int, Int, Double)
calculateStats [] = (0, 0, 0.0)
calculateStats xs = (minimum xs, maximum xs, average xs)
  where
    average ys = fromIntegral (sum ys) / fromIntegral (length ys)

-- Safe division with error handling
safeDivide :: Int -> Int -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (fromIntegral x / fromIntegral y)

-- Transform a list with a function and accumulate results
transformAndAccumulate :: (a -> b) -> [a] -> [b]
transformAndAccumulate f = foldr (\x acc -> f x : acc) []module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers