module DataProcessor where

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
  where
    windows :: Int -> [a] -> [[a]]
    windows m = takeWhile ((== m) . length) . map (take m) . iterate tail
    
    average :: [Double] -> Double
    average ys = sum ys / fromIntegral (length ys)

smoothData :: [Double] -> [Double]
smoothData = movingAverage 3

testData :: [Double]
testData = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]