
module DataProcessor where

import Data.List.Split (splitOn)

parseCSV :: String -> [[Double]]
parseCSV content = map (map read . splitOn ",") (lines content)

calculateAverages :: [[Double]] -> [Double]
calculateAverages rows = 
    if null rows then []
    else map (\col -> sum col / fromIntegral (length col)) (transpose rows)
  where
    transpose :: [[Double]] -> [[Double]]
    transpose [] = []
    transpose ([]:_) = []
    transpose xs = map head xs : transpose (map tail xs)

processCSVData :: String -> [Double]
processCSVData = calculateAverages . parseCSV