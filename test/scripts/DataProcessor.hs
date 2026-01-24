module DataProcessor where

import Data.List (tails)

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map avg $ filter (\w -> length w == n) $ tails xs
  where
    avg ws = sum ws / fromIntegral n

smoothData :: Fractional a => Int -> [a] -> [a]
smoothData window dataList = movingAverage window dataList

validateData :: (Ord a, Num a) => [a] -> Maybe String
validateData [] = Just "Empty dataset"
validateData xs
    | any (< 0) xs = Just "Negative values detected"
    | any isInfinite xs = Just "Infinite values detected"
    | otherwise = Nothing
  where
    isInfinite x = x == 1/0 || x == -1/0

processDataset :: Fractional a => Int -> [a] -> Either String [a]
processDataset window dataset =
    case validateData dataset of
        Just err -> Left err
        Nothing -> Right $ smoothData window dataset