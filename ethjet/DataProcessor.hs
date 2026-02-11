module DataProcessor where

import Data.List (tails)

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map avg $ filter (\w -> length w == n) $ tails xs
  where
    avg window = sum window / fromIntegral n

-- Example usage with test data
testData :: [Double]
testData = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]

main :: IO ()
main = do
    putStrLn "Original data:"
    print testData
    putStrLn "\n3-period moving average:"
    print $ movingAverage 3 testData
    putStrLn "\n5-period moving average:"
    print $ movingAverage 5 testData