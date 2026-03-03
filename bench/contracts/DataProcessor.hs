module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | length xs < n = []
    | otherwise = avg (take n xs) : movingAverage n (tail xs)
  where
    avg ys = sum ys / fromIntegral (length ys)

smoothData :: Fractional a => Int -> [a] -> [a]
smoothData windowSize dataPoints =
    let avg = movingAverage windowSize dataPoints
        padding = replicate (windowSize `div` 2) (head dataPoints)
    in padding ++ avg ++ padding

calculateTrend :: (Fractional a, Ord a) => [a] -> String
calculateTrend values
    | null values = "No data"
    | last values > head values = "Increasing trend"
    | last values < head values = "Decreasing trend"
    | otherwise = "Stable trend"

processDataset :: Fractional a => Int -> [a] -> ([a], String)
processDataset window dataset =
    let smoothed = smoothData window dataset
        trend = calculateTrend smoothed
    in (smoothed, trend)