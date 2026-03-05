
module DataProcessor where

import Data.Time
import Text.CSV

filterCSVByDate :: Day -> Day -> CSV -> CSV
filterCSVByDate startDate endDate csv = 
    filter (isWithinDateRange startDate endDate) csv

isWithinDateRange :: Day -> Day -> Record -> Bool
isWithinDateRange startDate endDate record =
    case parseDateFromRecord record of
        Just date -> date >= startDate && date <= endDate
        Nothing -> False

parseDateFromRecord :: Record -> Maybe Day
parseDateFromRecord record =
    case record of
        (dateStr:_) -> parseTimeM True defaultTimeLocale "%Y-%m-%d" dateStr
        _ -> Nothingmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = all (\x -> x >= -100 && x <= 100) xs

main :: IO ()
main = do
    let sampleData = [1, -2, 3, 0, 5, -10]
    if validateInput sampleData
        then do
            putStrLn "Processing valid data..."
            let result = processData sampleData
            putStrLn $ "Input: " ++ show sampleData
            putStrLn $ "Result: " ++ show result
        else
            putStrLn "Invalid input data detected"module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData = all (> 0)

main :: IO ()
main = do
    let sampleData = [1, -2, 3, 0, 5, -7]
    let processed = processData sampleData
    putStrLn $ "Original data: " ++ show sampleData
    putStrLn $ "Processed data: " ++ show processed
    putStrLn $ "Data validation: " ++ show (validateData processed)