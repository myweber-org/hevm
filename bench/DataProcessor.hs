module DataProcessor where

import Data.List.Split (splitOn)

parseCSV :: String -> [[Double]]
parseCSV csv = map (map read . splitOn ",") (lines csv)

computeAverages :: [[Double]] -> [Double]
computeAverages rows
  | null rows = []
  | otherwise = map average (transpose rows)
  where
    average xs = sum xs / fromIntegral (length xs)
    transpose = foldr (zipWith (:)) (repeat [])

processCSVData :: String -> [Double]
processCSVData = computeAverages . parseCSV