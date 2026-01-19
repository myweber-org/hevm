module DataProcessor where

import Data.List.Split (splitOn)

parseCSV :: String -> [[Double]]
parseCSV content = map (map read . splitOn ",") $ lines content

calculateAverages :: [[Double]] -> [Double]
calculateAverages rows = 
    if null rows then []
    else map (\col -> sum col / fromIntegral (length col)) $ transpose rows
  where
    transpose :: [[a]] -> [[a]]
    transpose [] = []
    transpose ([]:_) = []
    transpose x = map head x : transpose (map tail x)

processCSVData :: String -> Maybe [Double]
processCSVData csv = 
    let parsed = parseCSV csv
    in if all (\row -> length row == length (head parsed)) parsed
       then Just $ calculateAverages parsed
       else Nothing