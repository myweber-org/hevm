module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage windowSize xs
    | windowSize <= 0 = error "Window size must be positive"
    | length xs < windowSize = []
    | otherwise = map average $ windows windowSize xs
  where
    windows n = takeWhile ((== n) . length) . map (take n) . tails
    average ys = sum ys / fromIntegral (length ys)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Maybe [Int]
validateInput xs
    | null xs = Nothing
    | any (< -100) xs = Nothing
    | any (> 100) xs = Nothing
    | otherwise = Just xs