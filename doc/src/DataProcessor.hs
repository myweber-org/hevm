module DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter evenmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print result
module DataProcessor where

import Data.List.Split (splitOn)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ",") (lines content)

numericColumns :: CSVData -> [[Double]]
numericColumns [] = []
numericColumns (header:rows) = 
    map (parseRow . map (!!) rows) [0..length header - 1]
  where
    parseRow col = map (read . trim) col
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

columnAverage :: [[Double]] -> [Double]
columnAverage columns = map avg columns
  where
    avg col = if null col then 0 else sum col / fromIntegral (length col)

processCSVData :: String -> [Double]
processCSVData content = 
    let parsed = parseCSV content
        numeric = numericColumns parsed
    in columnAverage numeric

safeReadDouble :: String -> Maybe Double
safeReadDouble s = case reads s of
    [(n, "")] -> Just n
    _ -> Nothing

filterValidRows :: CSVData -> CSVData
filterValidRows rows = 
    filter (all (\cell -> not (null cell) && not (any (==',') cell))) rows