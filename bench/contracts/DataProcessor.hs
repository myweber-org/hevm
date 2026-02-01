
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

calculateAverage :: [Record] -> Double
calculateAverage records = 
  if null records 
    then 0.0
    else sum values / fromIntegral (length values)
  where
    values = map snd records

processData :: String -> (Double, Int)
processData csv = 
  let records = parseCSV csv
      avg = calculateAverage records
      count = length records
  in (avg, count)

main :: IO ()
main = do
  let sampleData = "Alice,85.5\nBob,92.0\nCharlie,78.5\nInvalidLine\nDiana,88.0"
  let (average, recordCount) = processData sampleData
  putStrLn $ "Average: " ++ show average
  putStrLn $ "Records processed: " ++ show recordCount
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

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

main :: IO ()
main = do
    let numbers = [-3, -1, 0, 2, 5, 8]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)