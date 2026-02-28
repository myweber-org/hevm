module DataProcessor where

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
  where
    windows :: Int -> [a] -> [[a]]
    windows m = takeWhile ((== m) . length) . map (take m) . tails
    
    average :: [Double] -> Double
    average ys = sum ys / fromIntegral (length ys)

smoothData :: [Double] -> [Double]
smoothData = movingAverage 3

calculateTrend :: [Double] -> Maybe Double
calculateTrend [] = Nothing
calculateTrend [_] = Nothing
calculateTrend values = Just slope
  where
    n = fromIntegral $ length values
    indices = [0..n-1]
    sumX = sum indices
    sumY = sum values
    sumXY = sum $ zipWith (*) indices values
    sumX2 = sum $ map (^2) indices
    slope = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [1, -2, 3, -4, 5]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbers
module DataProcessor where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Record = (String, Double, Double)

parseCSVLine :: String -> Maybe Record
parseCSVLine line = case splitOn "," line of
    [name, val1Str, val2Str] -> 
        case (reads val1Str, reads val2Str) of
            ([(val1, "")], [(val2, "")]) -> Just (name, val1, val2)
            _ -> Nothing
    _ -> Nothing

calculateAverages :: [Record] -> (Double, Double)
calculateAverages records =
    let (sum1, sum2, count) = foldr (\(_, v1, v2) (s1, s2, c) -> (s1 + v1, s2 + v2, c + 1)) (0, 0, 0) records
    in if count > 0 
        then (sum1 / fromIntegral count, sum2 / fromIntegral count)
        else (0, 0)

processCSVData :: String -> (Double, Double)
processCSVData csvContent =
    let lines' = filter (not . null) $ lines csvContent
        records = mapMaybe parseCSVLine lines'
    in calculateAverages records

filterRecordsByThreshold :: [Record] -> Double -> [Record]
filterRecordsByThreshold records threshold =
    filter (\(_, v1, v2) -> v1 > threshold || v2 > threshold) records
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processData

validateInput :: [Int] -> Maybe [Int]
validateInput xs
  | null xs = Nothing
  | otherwise = Just xs
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers