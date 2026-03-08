
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
    [dateStr, name, amountStr] -> do
        date <- parseDate dateStr
        amount <- readMaybe amountStr
        return (date, name, amount)
    _ -> Nothing
  where
    readMaybe :: String -> Maybe Double
    readMaybe s = case reads s of
        [(x, "")] -> Just x
        _ -> Nothing

filterByDateRange :: Day -> Day -> [Record] -> [Record]
filterByDateRange start end = filter (\(d, _, _) -> d >= start && d <= end)

loadRecords :: FilePath -> IO [Record]
loadRecords path = do
    content <- readFile path
    return $ mapMaybe parseRecord (lines content)

summarizeRecords :: [Record] -> [(String, Double)]
summarizeRecords records =
    map (\(name, amounts) -> (name, sum amounts)) $
    groupByNames records
  where
    groupByNames = foldl' insertRecord []
    insertRecord acc (_, name, amount) =
        case lookup name acc of
            Just total -> (name, total + amount) : delete (name, total) acc
            Nothing -> (name, amount) : acc

processData :: FilePath -> Day -> Day -> IO [(String, Double)]
processData path start end = do
    records <- loadRecords path
    let filtered = filterByDateRange start end records
    return $ summarizeRecords filtered