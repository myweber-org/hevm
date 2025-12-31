module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
  where
    windows m ys = take (length ys - m + 1) $ zipWith (++) (tails ys) (repeat [])
    average zs = sum zs / fromIntegral (length zs)

smoothData :: Fractional a => Int -> [a] -> [a]
smoothData windowSize dataList =
    let avg = movingAverage windowSize dataList
        padding = replicate (windowSize `div` 2) (head dataList)
    in padding ++ avg ++ padding