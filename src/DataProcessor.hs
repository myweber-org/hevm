module DataProcessor where

import Data.List (sort)

type DataPoint = (Int, Double)

filterByThreshold :: Double -> [DataPoint] -> [DataPoint]
filterByThreshold threshold = filter (\(_, value) -> value > threshold)

normalizeData :: [DataPoint] -> [DataPoint]
normalizeData points = map normalize points
  where
    maxVal = maximum $ map snd points
    normalize (id, val) = (id, val / maxVal)

calculateStatistics :: [DataPoint] -> (Double, Double, Double)
calculateStatistics points = (minimum values, maximum values, average values)
  where
    values = map snd points
    average xs = sum xs / fromIntegral (length xs)

sortByValue :: [DataPoint] -> [DataPoint]
sortByValue = sortOn snd

processDataPipeline :: Double -> [DataPoint] -> [DataPoint]
processDataPipeline threshold = 
    sortByValue . normalizeData . filterByThreshold threshold