module DataProcessor where

import Data.List (tails)

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | n > length xs = error "Window size exceeds list length"
    | otherwise = map average $ filter (\window -> length window == n) $ tails xs
  where
    average :: [Double] -> Double
    average ws = sum ws / fromIntegral (length ws)

-- Example usage:
-- movingAverage 3 [1.0, 2.0, 3.0, 4.0, 5.0] -> [2.0, 3.0, 4.0]