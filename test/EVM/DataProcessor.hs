
module DataProcessor where

import Data.Time
import Data.Time.Format
import Data.List
import Data.Char

parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

type Record = (Day, String, Double)

parseRecord :: String -> Maybe Record
parseRecord line = case words line of
    [dateStr, desc, amountStr] -> do
        date <- parseDate dateStr
        amount <- readMaybe amountStr
        return (date, desc, amount)
    _ -> Nothing
  where
    readMaybe :: String -> Maybe Double
    readMaybe s = case reads s of
        [(x, "")] -> Just x
        _ -> Nothing

filterByDateRange :: Day -> Day -> [Record] -> [Record]
filterByDateRange start end = filter (\(date, _, _) -> date >= start && date <= end)

summarizeRecords :: [Record] -> [(String, Double)]
summarizeRecords = map summarize . groupBy sameDesc . sortBy compareDesc
  where
    sameDesc (_, desc1, _) (_, desc2, _) = desc1 == desc2
    compareDesc (_, desc1, _) (_, desc2, _) = compare desc1 desc2
    summarize group@((_, desc, _):_) = (desc, sum $ map (\(_, _, a) -> a) group)

processData :: String -> Day -> Day -> Either String [(String, Double)]
processData input start end = do
    let records = mapMaybe parseRecord (lines input)
    if null records
        then Left "No valid records found"
        else Right $ summarizeRecords $ filterByDateRange start end recordsmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)