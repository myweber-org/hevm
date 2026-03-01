
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -100) xs then Just xs else Nothing
module DataProcessor where

import Data.Char (isDigit, isSpace)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)

-- | Validates if a string contains only digits
validateDigits :: String -> Bool
validateDigits = all isDigit

-- | Safely parses an integer, returns Nothing on failure
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

-- | Processes a list of potential number strings, returning valid integers
processNumbers :: [String] -> [Int]
processNumbers = catMaybes . map safeParseInt

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
processData input =
    let cleaned = map trim input
        numbers = processNumbers cleaned
        (total, count, avg) = calculateStats numbers
    in "Processed " ++ show count ++ " numbers. Total: " ++ show total ++ 
       ", Average: " ++ show avg