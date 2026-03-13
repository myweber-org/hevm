module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers
module DataProcessor where

import Data.List (intercalate)
import Data.List.Split (splitOn)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ",") (lines content)

computeColumnAverages :: CSVData -> [Double]
computeColumnAverages [] = []
computeColumnAverages rows@(header:_) = 
    map (computeAverage . map read) (transpose numericRows)
  where
    numericRows = map (filter (all isNumeric)) (tail rows)
    isNumeric str = case reads str :: [(Double, String)] of
        [(_, "")] -> True
        _         -> False

computeAverage :: [Double] -> Double
computeAverage [] = 0.0
computeAverage xs = sum xs / fromIntegral (length xs)

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:xss) = transpose xss
transpose ((x:xs):xss) = 
    (x : [h | (h:_) <- xss]) : transpose (xs : [t | (_:t) <- xss])

formatResults :: [Double] -> String
formatResults averages = 
    "Column averages:\n" ++ 
    intercalate "\n" (zipWith formatLine [1..] averages)
  where
    formatLine idx avg = "Column " ++ show idx ++ ": " ++ show avg

processCSVFile :: String -> IO ()
processCSVFile filename = do
    content <- readFile filename
    let parsed = parseCSV content
    let averages = computeColumnAverages parsed
    putStrLn $ formatResults averagesmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)