module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | length xs < n = []
    | otherwise = avg (take n xs) : movingAverage n (tail xs)
    where
        avg ys = sum ys / fromIntegral (length ys)

smoothData :: Fractional a => Int -> [a] -> [a]
smoothData windowSize dataPoints =
    let avg = movingAverage windowSize dataPoints
        padding = replicate (windowSize `div` 2) (head dataPoints)
    in padding ++ avg ++ padding

processDataSet :: Fractional a => [a] -> ([a], a, a)
processDataSet dataset =
    let smoothed = smoothData 5 dataset
        meanVal = sum smoothed / fromIntegral (length smoothed)
        variance = sum (map (\x -> (x - meanVal) ** 2) smoothed) 
                   / fromIntegral (length smoothed)
    in (smoothed, meanVal, variance)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    let result = processNumbers numbers
    print result