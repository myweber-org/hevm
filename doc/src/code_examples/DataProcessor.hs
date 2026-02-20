module DataProcessor where

import Data.List (sort, group, maximumBy)
import Data.Ord (comparing)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.Vector (Vector, toList)

type Record = (String, Int, Double)

parseCSV :: BL.ByteString -> Either String (Vector Record)
parseCSV bs = Csv.decode Csv.NoHeader bs

calculateMean :: [Double] -> Double
calculateMean xs = sum xs / fromIntegral (length xs)

calculateMedian :: [Double] -> Double
calculateMedian xs
    | null xs = 0.0
    | odd len = sorted !! mid
    | otherwise = (sorted !! (mid - 1) + sorted !! mid) / 2.0
  where
    sorted = sort xs
    len = length xs
    mid = len `div` 2

calculateMode :: [Int] -> Int
calculateMode xs
    | null xs = 0
    | otherwise = fst $ maximumBy (comparing snd) freqMap
  where
    freqMap = map (\g -> (head g, length g)) $ group $ sort xs

filterByThreshold :: Double -> [Record] -> [Record]
filterByThreshold threshold = filter (\(_, _, value) -> value > threshold)

summarizeData :: [Record] -> (Double, Double, Int, Int)
summarizeData records =
    let values = map (\(_, _, v) -> v) records
        categories = map (\(_, c, _) -> c) records
        meanVal = calculateMean values
        medianVal = calculateMedian values
        modeVal = calculateMode categories
        count = length records
    in (meanVal, medianVal, modeVal, count)

processData :: BL.ByteString -> Either String (Double, Double, Int, Int)
processData input = do
    parsed <- parseCSV input
    let records = toList parsed
    return $ summarizeData recordsmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

import Data.List (tails)

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map avg $ filter (\window -> length window == n) $ tails xs
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

processDataset :: Int -> [Double] -> (Double, [Double])
processDataset windowSize dataset =
    let smoothed = smoothData windowSize dataset
        trend = calculateTrend smoothed
    in (trend, smoothed)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print result