module DataProcessor where

import Data.List (tails)

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | n > length xs = error "Window size exceeds list length"
    | otherwise = map average $ filter (\w -> length w == n) $ tails xs
  where
    average ws = sum ws / fromIntegral n

smoothData :: Int -> [Double] -> [Double]
smoothData windowSize dataPoints = 
    movingAverage windowSize dataPoints ++ 
    replicate (windowSize - 1) (last dataPoints)

validateData :: [Double] -> Maybe [Double]
validateData [] = Nothing
validateData xs
    | any isNaN xs = Nothing
    | any isInfinite xs = Nothing
    | otherwise = Just xs

processDataStream :: Int -> [Double] -> Maybe [Double]
processDataStream windowSize rawData = do
    validData <- validateData rawData
    return $ smoothData windowSize validData