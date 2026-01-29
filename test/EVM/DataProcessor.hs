module DataProcessor where

import Data.List.Split (splitOn)

parseCSV :: String -> [[Double]]
parseCSV content = map (map read . splitOn ",") (lines content)

computeAverages :: [[Double]] -> [Double]
computeAverages rows
    | null rows = []
    | otherwise = map avg (transpose rows)
  where
    avg xs = sum xs / fromIntegral (length xs)
    transpose = foldr (zipWith (:)) (repeat [])

processCSVData :: String -> [Double]
processCSVData = computeAverages . parseCSV