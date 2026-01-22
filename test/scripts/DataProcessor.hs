module DataProcessor where

import Data.List.Split (splitOn)

parseCSV :: String -> [[Double]]
parseCSV csv = map (map read . splitOn ",") $ lines csv

calculateAverages :: [[Double]] -> [Double]
calculateAverages rows = 
    if null rows then [] 
    else map (\col -> sum col / fromIntegral (length col)) $ transpose rows
  where
    transpose :: [[Double]] -> [[Double]]
    transpose [] = []
    transpose ([]:_) = []
    transpose x = map head x : transpose (map tail x)

processCSVData :: String -> [Double]
processCSVData csv = calculateAverages $ parseCSV csv
module DataProcessor where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Record = (String, Double)

parseCSV :: String -> [Record]
parseCSV csvData = mapMaybe parseLine (lines csvData)
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
  where values = map snd records

filterAboveAverage :: [Record] -> [Record]
filterAboveAverage records = 
  filter (\(_, value) -> value > avg) records
  where avg = calculateAverage records

processCSVData :: String -> [Record]
processCSVData = filterAboveAverage . parseCSV
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedData :: (Int -> Bool) -> (Int -> Int) -> [Int] -> Int
sumProcessedData predicate transformer = sum . filterAndTransform predicate transformer

validateInput :: [Int] -> Maybe [Int]
validateInput [] = Nothing
validateInput xs = Just xs

main :: IO ()
main = do
    let sampleData = [1..10]
    case validateInput sampleData of
        Nothing -> putStrLn "Empty input list"
        Just dataList -> do
            putStrLn $ "Original data: " ++ show dataList
            putStrLn $ "Even squares: " ++ show (processEvenSquares dataList)
            putStrLn $ "Sum of even squares: " ++ 
                show (sumProcessedData even (\x -> x * x) dataList)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processEvenSquares

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list (even numbers squared): " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of processed values: " ++ show (sumProcessed numbers)