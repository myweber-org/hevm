module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | length xs < n = []
    | otherwise = avg (take n xs) : movingAverage n (tail xs)
  where
    avg ys = sum ys / fromIntegral (length ys)

-- Example usage with a helper function
exampleUsage :: IO ()
exampleUsage = do
    let dataSeries = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
    let windowSize = 3
    let result = movingAverage windowSize dataSeries
    putStrLn $ "Data series: " ++ show dataSeries
    putStrLn $ "Moving average (window=" ++ show windowSize ++ "): " ++ show result