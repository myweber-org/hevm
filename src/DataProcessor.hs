module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processDatamodule DataProcessor where

import Data.List (tails)

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map avg $ filter (\w -> length w == n) $ tails xs
  where
    avg window = sum window / fromIntegral n

smoothData :: Int -> [Double] -> [Double]
smoothData windowSize dataPoints =
    movingAverage windowSize dataPoints

calculateTrend :: [Double] -> Double
calculateTrend values =
    let n = fromIntegral $ length values
        indices = [0..n-1]
        sumX = sum indices
        sumY = sum values
        sumXY = sum $ zipWith (*) indices values
        sumX2 = sum $ map (^2) indices
        slope = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX)
    in slope