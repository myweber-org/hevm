
module DataProcessor where

movingAverage :: Int -> [Double] -> [Double]
movingAverage windowSize xs
    | windowSize <= 0 = error "Window size must be positive"
    | windowSize > length xs = []
    | otherwise = map average $ windows windowSize xs
  where
    windows n = takeWhile ((== n) . length) . map (take n) . tails
    average list = sum list / fromIntegral (length list)

smoothData :: [Double] -> [Double]
smoothData = movingAverage 3

calculateTrend :: [Double] -> Maybe Double
calculateTrend [] = Nothing
calculateTrend xs = Just (last xs - head xs)