
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show result
module DataProcessor where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Record = (String, Double)

parseCSV :: String -> [Record]
parseCSV csv = mapMaybe parseLine (lines csv)
  where
    parseLine line = case splitOn "," line of
      [name, valueStr] -> case reads valueStr of
        [(value, "")] -> Just (name, value)
        _ -> Nothing
      _ -> Nothing

calculateAverage :: [Record] -> Maybe Double
calculateAverage [] = Nothing
calculateAverage records = Just (sum values / fromIntegral (length values))
  where
    values = map snd records

processCSVData :: String -> Maybe Double
processCSVData csv = calculateAverage (parseCSV csv)

filterAboveAverage :: [Record] -> Maybe Double -> [Record]
filterAboveAverage records avg = case avg of
  Just threshold -> filter (\(_, v) -> v > threshold) records
  Nothing -> []

main :: IO ()
main = do
  let csvData = "Alice,85.5\nBob,92.0\nCharlie,78.5\nDiana,88.0\n"
  let records = parseCSV csvData
  let avg = calculateAverage records
  putStrLn $ "Parsed records: " ++ show records
  putStrLn $ "Average: " ++ show avg
  putStrLn $ "Above average: " ++ show (filterAboveAverage records avg)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbers