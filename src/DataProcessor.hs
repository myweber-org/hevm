module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (* 2)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    print result
module DataProcessor where

import Data.List.Split (splitOn)

parseCSV :: String -> [[Double]]
parseCSV csv = map (map read . splitOn ",") $ lines csv

calculateAverages :: [[Double]] -> [Double]
calculateAverages rows = map avg rows
  where
    avg xs = sum xs / fromIntegral (length xs)

processCSVData :: String -> [Double]
processCSVData = calculateAverages . parseCSV

main :: IO ()
main = do
  let csvData = "1.0,2.0,3.0\n4.0,5.0,6.0\n7.0,8.0,9.0"
  let averages = processCSVData csvData
  putStrLn "Averages per row:"
  mapM_ (putStrLn . show) averagesmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData = all (> 0)

main :: IO ()
main = do
    let sampleData = [1, -2, 3, -4, 5]
    putStrLn $ "Original data: " ++ show sampleData
    putStrLn $ "Processed data: " ++ show (processData sampleData)
    putStrLn $ "Data validation: " ++ show (validateData sampleData)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processData