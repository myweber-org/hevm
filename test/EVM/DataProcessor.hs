
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
sumPositiveDoubles = sum . processNumbers