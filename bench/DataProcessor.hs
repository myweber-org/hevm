module DataProcessor where

import Data.List.Split (splitOn)

parseCSV :: String -> [[Double]]
parseCSV csvData = map (map read . splitOn ",") $ lines csvData

calculateAverages :: [[Double]] -> [Double]
calculateAverages rows = 
    if null rows then []
    else map (\col -> sum col / fromIntegral (length col)) $ transpose rows
  where
    transpose :: [[Double]] -> [[Double]]
    transpose [] = []
    transpose ([]:_) = []
    transpose xss = map head xss : transpose (map tail xss)

processCSVData :: String -> [Double]
processCSVData csvContent = 
    let parsedData = parseCSV csvContent
    in calculateAverages parsedData

validateRowLengths :: [[Double]] -> Bool
validateRowLengths rows = 
    case rows of
        [] -> True
        (first:rest) -> all ((== length first) . length) rest