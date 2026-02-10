
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)
module DataProcessor where

import Data.Time
import Data.Time.Format
import Data.List
import Data.Maybe
import Text.Read

type Record = (Day, String, Double)

parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

parseRecord :: String -> Maybe Record
parseRecord line = case words line of
    [dateStr, label, valueStr] -> do
        date <- parseDate dateStr
        value <- readMaybe valueStr
        return (date, label, value)
    _ -> Nothing

filterByDateRange :: Day -> Day -> [Record] -> [Record]
filterByDateRange start end = filter (\(d, _, _) -> d >= start && d <= end)

loadRecords :: FilePath -> IO [Record]
loadRecords path = do
    content <- readFile path
    return $ catMaybes $ map parseRecord (lines content)

summarizeRecords :: [Record] -> [(String, Double)]
summarizeRecords records =
    let groups = groupBy (\(_, l1, _) (_, l2, _) -> l1 == l2) $ sortOn (\(_, l, _) -> l) records
    in map (\g -> let (_, label, _) = head g in (label, sum $ map (\(_, _, v) -> v) g)) groups

processDataFile :: FilePath -> Day -> Day -> IO [(String, Double)]
processDataFile path start end = do
    records <- loadRecords path
    let filtered = filterByDateRange start end records
    return $ summarizeRecords filtered