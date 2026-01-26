
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)module DataProcessor where

import Data.List (tails)

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | n > length xs = error "Window size exceeds list length"
    | otherwise = map avg $ filter (\window -> length window == n) $ tails xs
  where
    avg window = sum window / fromIntegral n

smoothData :: Fractional a => Int -> [a] -> [a]
smoothData windowSize dataPoints =
    movingAverage windowSize dataPoints

validateData :: (Ord a, Num a) => [a] -> Maybe String
validateData [] = Just "Empty data set"
validateData xs
    | any (< 0) xs = Just "Negative values found"
    | any isInfinite xs = Just "Infinite values found"
    | any isNaN xs = Just "NaN values found"
    | otherwise = Nothing
  where
    isInfinite x = x > 1e100 || x < -1e100

processDataSet :: Fractional a => Int -> [a] -> Either String [a]
processDataSet windowSize dataPoints =
    case validateData dataPoints of
        Just err -> Left err
        Nothing -> Right $ smoothData windowSize dataPoints