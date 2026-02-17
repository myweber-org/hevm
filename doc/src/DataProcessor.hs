module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage _ [] = []
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | n > length xs = []
    | otherwise = map avg $ windows n xs
  where
    windows m ys = take (length ys - m + 1) $ iterate (drop 1) ys
    avg zs = sum zs / fromIntegral (length zs)

smoothData :: Fractional a => Int -> [a] -> [a]
smoothData windowSize dataPoints =
    movingAverage windowSize dataPoints