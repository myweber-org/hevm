module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
  where
    windows :: Int -> [a] -> [[a]]
    windows m ys = take (length ys - m + 1) $ zipWith (const . take m) (tails ys) ys
    
    average :: Fractional a => [a] -> a
    average zs = sum zs / fromIntegral (length zs)

-- Example usage:
-- movingAverage 3 [1,2,3,4,5] == [2.0,3.0,4.0]