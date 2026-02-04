
module DataProcessor where

import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)

type Record = (String, Double, Double)

parseCSV :: String -> [Record]
parseCSV content = catMaybes $ map parseLine (lines content)
  where
    parseLine line = case splitOn "," line of
      [name, val1, val2] -> 
        case (reads val1, reads val2) of
          ([(v1, "")], [(v2, "")]) -> Just (name, v1, v2)
          _ -> Nothing
      _ -> Nothing

calculateAverages :: [Record] -> (Double, Double)
calculateAverages records = (avg val1s, avg val2s)
  where
    (val1s, val2s) = unzip $ map (\(_, v1, v2) -> (v1, v2)) records
    avg xs = sum xs / fromIntegral (length xs)

processData :: String -> (Double, Double)
processData = calculateAverages . parseCSV

filterByThreshold :: Double -> [Record] -> [Record]
filterByThreshold threshold = filter (\(_, v1, v2) -> v1 > threshold && v2 > threshold)