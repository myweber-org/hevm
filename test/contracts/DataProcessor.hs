module DataProcessor where

import Data.List (tails)

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | n > length xs = []
    | otherwise = map avg $ filter (\window -> length window == n) $ tails xs
  where
    avg window = sum window / fromIntegral n

safeMovingAverage :: Fractional a => Int -> [a] -> Maybe [a]
safeMovingAverage n xs
    | n <= 0 = Nothing
    | otherwise = Just $ movingAverage n xs

testData :: [Double]
testData = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]