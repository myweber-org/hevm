
module DataProcessor where

import Data.Time
import Text.CSV

filterByDateRange :: Day -> Day -> [Record] -> [Record]
filterByDateRange startDate endDate records = 
    filter (\record -> 
        case parseDate (head record) of
            Just date -> date >= startDate && date <= endDate
            Nothing -> False
    ) records
  where
    parseDate :: String -> Maybe Day
    parseDate str = 
        case parseTimeM True defaultTimeLocale "%Y-%m-%d" str of
            Just day -> Just day
            Nothing -> Nothing

calculateDailyAverage :: [Record] -> [(Day, Double)]
calculateDailyAverage records =
    let grouped = groupByDate records
    in map (\(date, values) -> (date, average values)) grouped
  where
    groupByDate :: [Record] -> [(Day, [Double])]
    groupByDate = foldr (\record acc ->
        case (parseDate (head record), parseDouble (record !! 1)) of
            (Just date, Just value) -> 
                let (same, rest) = partition (\(d, _) -> d == date) acc
                in case same of
                    [] -> (date, [value]) : rest
                    ((_, vals):_) -> (date, value:vals) : rest
            _ -> acc
    ) []
    
    parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"
    parseDouble str = readMaybe str
    
    average :: [Double] -> Double
    average xs = sum xs / fromIntegral (length xs)