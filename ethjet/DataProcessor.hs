module DataProcessor where

import Data.List.Split (splitOn)

parseCSV :: String -> [[Double]]
parseCSV content = map (map read . splitOn ",") $ lines content

calculateAverages :: [[Double]] -> [Double]
calculateAverages rows
    | null rows = []
    | otherwise = map avg $ transpose rows
  where
    avg xs = sum xs / fromIntegral (length xs)
    transpose = foldr (zipWith (:)) (repeat [])

processCSVData :: String -> [Double]
processCSVData = calculateAverages . parseCSV

validateRowLengths :: [[Double]] -> Bool
validateRowLengths rows = all ((== expectedLength) . length) rows
  where
    expectedLength
        | null rows = 0
        | otherwise = length (head rows)