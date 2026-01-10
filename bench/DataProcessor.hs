module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData = all (> 0)

main :: IO ()
main = do
    let sampleData = [-3, 1, 0, 5, -2, 8]
    putStrLn $ "Original data: " ++ show sampleData
    
    let processed = processData sampleData
    putStrLn $ "Processed data: " ++ show processed
    
    putStrLn $ "Data validation: " ++ show (validateData processed)module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | length xs < n = []
    | otherwise = avg : movingAverage n (tail xs)
    where
        window = take n xs
        avg = sum window / fromIntegral n

smoothData :: Fractional a => Int -> [a] -> [a]
smoothData windowSize dataPoints =
    let padded = replicate (windowSize `div` 2) (head dataPoints) ++ dataPoints ++ replicate (windowSize `div` 2) (last dataPoints)
    in movingAverage windowSize padded

processNumericStream :: (Fractional a, Ord a) => Int -> [a] -> [(a, Bool)]
processNumericStream threshold values =
    let averages = smoothData 5 values
        isAboveThreshold x = x > fromIntegral threshold
    in zip averages (map isAboveThreshold averages)