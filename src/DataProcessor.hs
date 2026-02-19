
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processNumbers input
    putStrLn $ "Original list: " ++ show input
    putStrLn $ "Processed list: " ++ show resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)
module DataProcessor where

import Data.Char (isDigit, isSpace)
import Data.List (intercalate)

type ValidationRule = String -> Bool
type Transformation = String -> String

validateNumeric :: ValidationRule
validateNumeric = all (\c -> isDigit c || c == '.')

validateNonEmpty :: ValidationRule
validateNonEmpty = not . all isSpace

trimWhitespace :: Transformation
trimWhitespace = reverse . dropWhile isSpace . reverse . dropWhile isSpace

toUpperCase :: Transformation
toUpperCase = map toUpper

processCSVRow :: [ValidationRule] -> [Transformation] -> [String] -> Either String [String]
processCSVRow validators transformers row
    | length row /= length validators = Left "Column count mismatch"
    | any (== False) validationResults = Left $ "Validation failed: " ++ formatErrors validationResults
    | otherwise = Right transformedRow
  where
    validationResults = zipWith ($) validators row
    transformedRow = zipWith ($) transformers row
    formatErrors = intercalate ", " . map snd . filter (not . fst) . zip validationResults . map (("Column " ++) . show) $ [1..]

safeReadDouble :: String -> Either String Double
safeReadDouble str
    | validateNumeric str = Right (read str)
    | otherwise = Left $ "Invalid numeric format: " ++ str

processNumericColumn :: String -> Either String Double
processNumericColumn = safeReadDouble . trimWhitespacemodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (^2)

sumProcessed :: (Int -> Bool) -> (Int -> Int) -> [Int] -> Int
sumProcessed predicate transformer = 
    sum . filterAndTransform predicate transformermodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processDatamodule DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | length xs < n = []
    | otherwise = avg : movingAverage n (tail xs)
    where
        window = take n xs
        avg = sum window / fromIntegral n

smoothData :: Fractional a => Int -> [a] -> [a]
smoothData windowSize dataPoints =
    let padSize = windowSize `div` 2
        padded = replicate padSize (head dataPoints) ++ dataPoints ++ replicate padSize (last dataPoints)
    in movingAverage windowSize padded

processSensorReadings :: [Double] -> [Double]
processSensorReadings readings =
    let cleaned = filter (\x -> x >= 0 && x <= 100) readings
    in if null cleaned then [] else smoothData 5 cleaned
module DataProcessor where

import Data.Char (isDigit, isAlpha)
import Data.List (intercalate)

-- Validate if a string contains only digits
validateNumeric :: String -> Bool
validateNumeric = all isDigit

-- Validate if a string contains only alphabetic characters
validateAlpha :: String -> Bool
validateAlpha = all isAlpha

-- Transform a list of strings to uppercase
transformToUpper :: [String] -> [String]
transformToUpper = map (map toUpper)

-- Filter out invalid numeric strings from a list
filterValidNumbers :: [String] -> [String]
filterValidNumbers = filter validateNumeric

-- Join a list of strings with a separator
joinWithSeparator :: String -> [String] -> String
joinWithSeparator separator = intercalate separator

-- Process a list by validating numbers and joining valid ones
processData :: [String] -> String
processData input = 
    let validNumbers = filterValidNumbers input
    in joinWithSeparator ", " validNumbers

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
    let testData = ["123", "abc", "456", "def789", "101112"]
    putStrLn "Original data:"
    print testData
    putStrLn "\nValid numeric data:"
    print $ filterValidNumbers testData
    putStrLn "\nProcessed result:"
    putStrLn $ processData testDatamodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print result