module DataProcessor where

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
    let avg = movingAverage windowSize dataPoints
        padding = replicate (windowSize `div` 2) (head avg)
    in padding ++ avg ++ padding

calculateTrend :: [Double] -> Double
calculateTrend [] = 0.0
calculateTrend points =
    let n = fromIntegral $ length points
        indices = [0..n-1]
        sumX = sum indices
        sumY = sum points
        sumXY = sum $ zipWith (*) indices points
        sumX2 = sum $ map (^2) indices
        slope = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX)
    in slope

processDataset :: Int -> [Double] -> (Double, [Double])
processDataset windowSize dataset =
    let smoothed = smoothData windowSize dataset
        trend = calculateTrend smoothed
    in (trend, smoothed)module DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter (>0)