module DataProcessor where

movingAverage :: Int -> [Double] -> [Double]
movingAverage windowSize xs
    | windowSize <= 0 = error "Window size must be positive"
    | length xs < windowSize = []
    | otherwise = map average $ windows windowSize xs
  where
    windows n = takeWhile (\w -> length w == n) . map (take n) . tails
    average ws = sum ws / fromIntegral (length ws)