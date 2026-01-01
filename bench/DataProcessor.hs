
module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
  where
    windows m ys = take (length ys - m + 1) $ iterate (tail) ys
    average zs = sum zs / fromIntegral n

smoothData :: Fractional a => Int -> [a] -> [a]
smoothData windowSize dataPoints =
    let avg = movingAverage windowSize dataPoints
        padding = replicate (windowSize `div` 2) (head dataPoints)
    in padding ++ avg ++ padding