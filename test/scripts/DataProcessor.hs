module DataProcessor where

import Data.List (tails)

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | n > length xs = []
    | otherwise = map avg $ filter (\window -> length window == n) $ tails xs
  where
    avg window = sum window / fromIntegral n

-- Example usage:
-- movingAverage 3 [1,2,3,4,5] -> [2.0,3.0,4.0]module DataProcessor where

import Data.List.Split (splitOn)

parseCSV :: String -> [[Double]]
parseCSV content = map (map read . splitOn ",") (lines content)

calculateAverages :: [[Double]] -> [Double]
calculateAverages rows
    | null rows = []
    | otherwise = map avg (transpose rows)
  where
    avg xs = sum xs / fromIntegral (length xs)
    transpose = foldr (zipWith (:)) (repeat [])

processCSVData :: String -> [Double]
processCSVData = calculateAverages . parseCSV