
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformFunction xs =
    map transformFunction (filter predicate xs)

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processEvenSquares

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (>0) xs then Just xs else Nothing

main :: IO ()
main = do
    let sampleData = [1,2,3,4,5,6]
    case validateInput sampleData of
        Just validData -> do
            putStrLn $ "Original data: " ++ show validData
            putStrLn $ "Processed data: " ++ show (processEvenSquares validData)
            putStrLn $ "Sum of processed data: " ++ show (sumProcessedData validData)
        Nothing -> putStrLn "Invalid input: all numbers must be positive"module DataProcessor where

import Data.List (tails)

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ filter (\window -> length window == n) $ tails xs
  where
    average :: Fractional a => [a] -> a
    average ws = sum ws / fromIntegral (length ws)

smoothData :: Fractional a => Int -> [a] -> [a]
smoothData windowSize dataSeries = movingAverage windowSize dataSeries

calculateTrend :: Fractional a => [a] -> Maybe a
calculateTrend [] = Nothing
calculateTrend [_] = Nothing
calculateTrend values = Just slope
  where
    n = fromIntegral $ length values
    indices = [0..n-1]
    sumX = sum indices
    sumY = sum values
    sumXY = sum $ zipWith (*) indices values
    sumX2 = sum $ map (^2) indices
    slope = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX)