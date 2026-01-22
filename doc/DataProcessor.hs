module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | length xs < n = []
    | otherwise = avg : movingAverage n (tail xs)
  where
    window = take n xs
    avg = sum window / fromIntegral n

smoothData :: Fractional a => Int -> [a] -> [a]
smoothData windowSize dataPoints =
    let prefix = replicate (windowSize `div` 2) (head dataPoints)
        suffix = replicate (windowSize `div` 2) (last dataPoints)
        extendedData = prefix ++ dataPoints ++ suffix
    in movingAverage windowSize extendedData

calculateTrend :: (Fractional a, Ord a) => [a] -> String
calculateTrend values
    | allIncreasing values = "Increasing trend"
    | allDecreasing values = "Decreasing trend"
    | otherwise = "No clear trend"
  where
    allIncreasing xs = and $ zipWith (<=) xs (tail xs)
    allDecreasing xs = and $ zipWith (>=) xs (tail xs)