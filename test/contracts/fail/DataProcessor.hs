module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

import Data.List.Split (splitOn)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ",") (lines content)

extractNumericColumn :: CSVData -> Int -> [Double]
extractNumericColumn rows colIndex = 
    [ read cell | row <- rows, 
                  length row > colIndex, 
                  let cell = row !! colIndex, 
                  all (\c -> c `elem` "0123456789.-") cell ]

calculateAverage :: [Double] -> Double
calculateAverage [] = 0.0
calculateAverage xs = sum xs / fromIntegral (length xs)

processCSVFile :: String -> Int -> IO Double
processCSVFile filePath columnIndex = do
    content <- readFile filePath
    let parsedData = parseCSV content
    let numericData = extractNumericColumn parsedData columnIndex
    return $ calculateAverage numericDatamodule DataProcessor where

processData :: [Int] -> [Int]
processData xs = map (^2) (filter even xs)module DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter (>0)