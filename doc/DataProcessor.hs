module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, 0, 5, -8]
    let result = processData input
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let input = [1..10]
    let result = processData input
    print result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -100) xs then Just xs else Nothing

main :: IO ()
main = do
    let inputData = [1, -2, 3, 4, -5, 6]
    case validateInput inputData of
        Just validData -> do
            let result = sumProcessedData validData
            putStrLn $ "Sum of processed data: " ++ show result
        Nothing -> putStrLn "Invalid input data detected"module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Bool
validateInput = all (\x -> x >= -100 && x <= 100)

safeProcessData :: [Int] -> Maybe [Int]
safeProcessData xs
    | validateInput xs = Just (processData xs)
    | otherwise = Nothing
module DataProcessor where

import Data.Char (isDigit)
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)

-- Safe integer parsing with validation
safeParseInt :: String -> Maybe Int
safeParseInt str
    | all isDigit str = readMaybe str
    | otherwise = Nothing

-- Process list of strings to valid integers
processNumbers :: [String] -> [Int]
processNumbers = catMaybes . map safeParseInt

-- Validate and transform key-value pairs
validateKeyValue :: [(String, String)] -> [(String, Int)]
validateKeyValue pairs = 
    [(key, val) | (key, strVal) <- pairs, 
                  Just val <- [safeParseInt strVal]]

-- Calculate statistics from processed data
calculateStats :: [Int] -> (Int, Int, Double)
calculateStats nums
    | null nums = (0, 0, 0.0)
    | otherwise = (minimum nums, maximum nums, average)
    where
        total = sum nums
        count = length nums
        average = fromIntegral total / fromIntegral count

-- Main processing pipeline
processDataPipeline :: [String] -> Maybe (Int, Int, Double)
processDataPipeline inputs
    | null validNumbers = Nothing
    | otherwise = Just stats
    where
        validNumbers = processNumbers inputs
        stats = calculateStats validNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processData
module DataProcessor where

import Data.Time
import Text.CSV

filterCSVByDate :: String -> Day -> Day -> Either String [Record]
filterCSVByDate csvContent startDate endDate = do
    csv <- parseCSV "input" csvContent
    let filtered = filter (isWithinDateRange startDate endDate) csv
    return filtered

isWithinDateRange :: Day -> Day -> Record -> Bool
isWithinDateRange start end record =
    case record of
        (dateStr:_) -> 
            case parseDate dateStr of
                Just date -> date >= start && date <= end
                Nothing -> False
        _ -> False

parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"