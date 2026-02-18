module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData
module DataProcessor where

import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)

type Record = (String, Double)

parseCSV :: String -> [Record]
parseCSV content = catMaybes $ map parseLine (lines content)
  where
    parseLine line = case splitOn "," line of
      [name, valueStr] -> case reads valueStr of
        [(value, "")] -> Just (name, value)
        _ -> Nothing
      _ -> Nothing

calculateAverage :: [Record] -> Double
calculateAverage records = 
  if null records 
    then 0.0 
    else total / fromIntegral (length records)
  where
    total = sum $ map snd records

filterAboveAverage :: [Record] -> [Record]
filterAboveAverage records = 
  filter (\(_, value) -> value > avg) records
  where
    avg = calculateAverage records

processCSVData :: String -> [Record]
processCSVData = filterAboveAverage . parseCSVmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processNumbers input
    print result