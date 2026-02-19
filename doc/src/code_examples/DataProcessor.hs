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
processData = filterAndTransform (> 0) (* 2)