module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processDatamodule DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | length xs < n = []
    | otherwise = avg : movingAverage n (tail xs)
  where
    window = take n xs
    avg = sum window / fromIntegral n

smoothData :: Fractional a => Int -> [a] -> [a]
smoothData windowSize dataPoints =
    let prefix = replicate (windowSize `div` 2) (head dataPoints)
        suffix = replicate (windowSize `div` 2) (last dataPoints)
        extendedData = prefix ++ dataPoints ++ suffix
    in movingAverage windowSize extendedData

calculateTrend :: (Fractional a, Ord a) => [a] -> String
calculateTrend values
    | allIncreasing = "Increasing trend"
    | allDecreasing = "Decreasing trend"
    | otherwise = "No clear trend"
  where
    pairs = zip values (tail values)
    allIncreasing = all (uncurry (<)) pairs
    allDecreasing = all (uncurry (>)) pairs

processDataset :: Fractional a => Int -> [a] -> ([a], String)
processDataset windowSize dataset =
    let smoothed = smoothData windowSize dataset
        trend = calculateTrend smoothed
    in (smoothed, trend)