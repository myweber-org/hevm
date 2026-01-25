
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
calculateAverage records
    | null records = 0.0
    | otherwise = total / count
  where
    (total, count) = foldl (\(sumAcc, cnt) (_, val) -> (sumAcc + val, cnt + 1)) (0.0, 0) records

filterByThreshold :: Double -> [Record] -> [Record]
filterByThreshold threshold = filter (\(_, val) -> val > threshold)

processData :: String -> Double -> [Record]
processData csvData threshold = 
    filterByThreshold threshold $ parseCSV csvData

main :: IO ()
main = do
    let testData = "Alice,85.5\nBob,92.0\nCharlie,78.5\nDiana,88.0\n"
    let threshold = 80.0
    let filtered = processData testData threshold
    putStrLn "Filtered records:"
    mapM_ (\(name, val) -> putStrLn $ name ++ ": " ++ show val) filtered
    putStrLn $ "Average: " ++ show (calculateAverage filtered)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print result