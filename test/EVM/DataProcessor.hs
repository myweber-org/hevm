module DataProcessor where

import Data.List.Split (splitOn)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ",") (lines content)

numericFields :: Row -> [Double]
numericFields row = map read (filter isNumeric row)
  where
    isNumeric s = case reads s :: [(Double, String)] of
      [(_, "")] -> True
      _         -> False

calculateAverages :: CSVData -> [Double]
calculateAverages csvData =
  let numericRows = map numericFields csvData
      transposed = transpose numericRows
  in map average transposed
  where
    transpose :: [[Double]] -> [[Double]]
    transpose [] = []
    transpose ([]:_) = []
    transpose xss = map head xss : transpose (map tail xss)
    
    average :: [Double] -> Double
    average [] = 0.0
    average xs = sum xs / fromIntegral (length xs)

processCSVFile :: String -> IO [Double]
processCSVFile filename = do
  content <- readFile filename
  let parsed = parseCSV content
  return (calculateAverages parsed)