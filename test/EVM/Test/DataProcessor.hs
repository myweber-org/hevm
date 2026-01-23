
module DataProcessor where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Record = (String, Double)

parseCSVLine :: String -> Maybe Record
parseCSVLine line = case splitOn "," line of
    [name, valueStr] -> case reads valueStr of
        [(value, "")] -> Just (name, value)
        _ -> Nothing
    _ -> Nothing

parseCSV :: String -> [Record]
parseCSV = mapMaybe parseCSVLine . lines

calculateAverage :: [Record] -> Double
calculateAverage records = 
    if null records 
        then 0.0 
        else total / fromIntegral count
  where
    (total, count) = foldl (\(sum, cnt) (_, val) -> (sum + val, cnt + 1)) (0.0, 0) records

filterByThreshold :: Double -> [Record] -> [Record]
filterByThreshold threshold = filter (\(_, value) -> value > threshold)

processData :: String -> Double -> (Double, [Record])
processData csvData threshold = 
    let records = parseCSV csvData
        average = calculateAverage records
        filtered = filterByThreshold threshold records
    in (average, filtered)