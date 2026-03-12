module DataProcessor where

movingAverage :: (Fractional a) => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
    where
        windows :: Int -> [a] -> [[a]]
        windows m = takeWhile ((== m) . length) . map (take m) . iterate tail
        
        average :: (Fractional a) => [a] -> a
        average ys = sum ys / fromIntegral (length ys)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)