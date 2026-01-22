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
parseDate str = parseTimeM True defaultTimeLocale "%Y-%m-%d" str