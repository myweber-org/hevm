
module DataProcessor where

import Data.List (foldl')
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.Vector (Vector)
import qualified Data.Vector as V

type Record = (String, Double, Int)

parseCSV :: BL.ByteString -> Either String (Vector Record)
parseCSV input = do
    decoded <- Csv.decode Csv.NoHeader input
    return $ V.map toRecord decoded
  where
    toRecord (name, value, count) = (name, value, count)

calculateStats :: Vector Record -> (Double, Double, Double)
calculateStats records = (avgValue, totalCount, maxValue)
  where
    values = V.map (\(_, v, _) -> v) records
    counts = V.map (\(_, _, c) -> fromIntegral c) records
    
    avgValue = if V.null values then 0.0 else V.sum values / fromIntegral (V.length values)
    totalCount = V.sum counts
    maxValue = if V.null values then 0.0 else V.maximum values

filterByThreshold :: Double -> Vector Record -> Vector Record
filterByThreshold threshold = V.filter (\(_, v, _) -> v > threshold)

processData :: BL.ByteString -> Double -> Either String (Vector Record, (Double, Double, Double))
processData csvData threshold = do
    records <- parseCSV csvData
    let filtered = filterByThreshold threshold records
    let stats = calculateStats filtered
    return (filtered, stats)module DataProcessor where

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