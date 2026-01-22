module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbersmodule DataProcessor where

processData :: [Int] -> [Int]
processData xs = map (^2) (filter even xs)
module DataProcessor where

import Data.Time
import Text.CSV

filterCSVByDate :: String -> Day -> Day -> Either String [Record]
filterCSVByDate csvContent startDate endDate =
    case parseCSV "" csvContent of
        Left err -> Left $ "CSV parse error: " ++ err
        Right csv ->
            let filtered = filter (isWithinDateRange startDate endDate) (tail csv)
            in Right filtered

isWithinDateRange :: Day -> Day -> Record -> Bool
isWithinDateRange start end row =
    case parseDate (head row) of
        Just date -> date >= start && date <= end
        Nothing -> False

parseDate :: String -> Maybe Day
parseDate str = parseTimeM True defaultTimeLocale "%Y-%m-%d" strmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

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