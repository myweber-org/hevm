module DataProcessor where

import Data.List (tails)

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map avg $ filter (\window -> length window == n) $ tails xs
  where
    avg window = sum window / fromIntegral n

-- Example usage:
-- movingAverage 3 [1.0, 2.0, 3.0, 4.0, 5.0] -> [2.0, 3.0, 4.0]