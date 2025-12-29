module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
  where
    windows m ys = take (length ys - m + 1) $ zipWith (++) (tails ys) (repeat [])
    average zs = sum zs / fromIntegral (length zs)

-- Alternative implementation using list comprehensions
movingAverage' :: Fractional a => Int -> [a] -> [a]
movingAverage' n xs
    | n <= 0 = error "Window size must be positive"
    | otherwise = [sum window / fromIntegral n | window <- windows n xs]
  where
    windows m ys = [take m (drop i ys) | i <- [0..length ys - m]]

-- Helper function to test with sample data
testMovingAverage :: IO ()
testMovingAverage = do
    let testData = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
    putStrLn "Testing moving average with window size 3:"
    print $ movingAverage 3 testData
    putStrLn "Testing moving average with window size 5:"
    print $ movingAverage 5 testData