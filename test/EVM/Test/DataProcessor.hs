module DataProcessor where

import Data.List (tails)

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | n > length xs = error "Window size exceeds list length"
    | otherwise = map average $ filter (\window -> length window == n) $ tails xs
  where
    average :: [Double] -> Double
    average window = sum window / fromIntegral n

-- Helper function to demonstrate usage
exampleData :: [Double]
exampleData = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]

main :: IO ()
main = do
    putStrLn "Original data:"
    print exampleData
    putStrLn "\n3-period moving average:"
    print $ movingAverage 3 exampleData
    putStrLn "\n5-period moving average:"
    print $ movingAverage 5 exampleData