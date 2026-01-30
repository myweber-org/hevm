module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage windowSize xs
    | windowSize <= 0 = error "Window size must be positive"
    | length xs < windowSize = []
    | otherwise = map avg $ windows windowSize xs
  where
    windows n = takeWhile ((== n) . length) . map (take n) . tails
    avg vals = sum vals / fromIntegral (length vals)