module DataProcessor where

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
  where
    windows :: Int -> [a] -> [[a]]
    windows m = takeWhile ((== m) . length) . map (take m) . tails
    
    average :: [Double] -> Double
    average ys = sum ys / fromIntegral (length ys)

smoothData :: [Double] -> [Double]
smoothData = movingAverage 3

calculateTrend :: [Double] -> Maybe Double
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
    slope = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [1, -2, 3, -4, 5]
    let result = processNumbers numbers
    print result