module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
    where
        windows :: Int -> [a] -> [[a]]
        windows size list = takeWhile ((== size) . length) $ map (take size) (tails list)
        
        average :: Fractional a => [a] -> a
        average vals = sum vals / fromIntegral (length vals)