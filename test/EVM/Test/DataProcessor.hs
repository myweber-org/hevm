module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage _ [] = []
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | n > length xs = []
    | otherwise = map average $ windows n xs
  where
    average ys = sum ys / fromIntegral (length ys)
    windows m = takeWhile ((== m) . length) . map (take m) . tailsmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers