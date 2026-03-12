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