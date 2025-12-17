
module DataProcessor where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Record = (String, Double, Double)

parseCSV :: String -> [Record]
parseCSV content = mapMaybe parseLine (lines content)
  where
    parseLine line = case splitOn "," line of
      [name, val1, val2] -> 
        case (reads val1, reads val2) of
          ([(v1, "")], [(v2, "")]) -> Just (name, v1, v2)
          _ -> Nothing
      _ -> Nothing

calculateAverages :: [Record] -> (Double, Double)
calculateAverages records = 
  let (sum1, sum2, count) = foldr (\(_, v1, v2) (s1, s2, c) -> (s1 + v1, s2 + v2, c + 1)) (0, 0, 0) records
  in if count > 0 then (sum1 / fromIntegral count, sum2 / fromIntegral count) else (0, 0)

processData :: String -> (Double, Double)
processData = calculateAverages . parseCSV

sampleData :: String
sampleData = 
  "Alice,85.5,92.0\n" ++
  "Bob,78.0,88.5\n" ++
  "Charlie,92.5,95.0\n" ++
  "Diana,88.0,91.5"