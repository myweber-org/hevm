
module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type CSVRow = [String]

parseCSV :: String -> [CSVRow]
parseCSV content = map (splitOn ',') (lines content)
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitHelper [""]
      where
        splitHelper :: Char -> [String] -> [String]
        splitHelper ch (x:xs)
          | ch == delimiter = "":x:xs
          | otherwise = (ch:x):xs

filterRows :: (CSVRow -> Bool) -> [CSVRow] -> [CSVRow]
filterRows predicate = filter predicate

containsOnlyDigits :: String -> Bool
containsOnlyDigits = all isDigit

filterNumericColumns :: CSVRow -> Bool
filterNumericColumns row = any containsOnlyDigits row

processCSVData :: String -> String
processCSVData input =
  let rows = parseCSV input
      filtered = filterRows filterNumericColumns rows
      outputRows = map (intercalate ",") filtered
  in unlines outputRows

main :: IO ()
main = do
  let sampleData = "name,age,city\nAlice,30,London\nBob,xyz,Paris\nCharlie,25,Berlin"
  putStrLn $ processCSVData sampleDatamodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = all (\x -> x >= -100 && x <= 100) xs

main :: IO ()
main = do
    let sampleData = [1, -2, 3, 0, 5, -8]
    if validateInput sampleData
        then do
            putStrLn "Processing valid data..."
            let result = processData sampleData
            putStrLn $ "Input: " ++ show sampleData
            putStrLn $ "Result: " ++ show result
        else
            putStrLn "Invalid input data detected"module DataProcessor where

import Data.List.Split (splitOn)
import Text.Read (readMaybe)

type Row = [Double]
type Dataset = [Row]

parseCSV :: String -> Maybe Dataset
parseCSV content = mapM parseRow (lines content)
  where
    parseRow line = mapM readMaybe (splitOn "," line)

calculateAverages :: Dataset -> Maybe [Double]
calculateAverages [] = Nothing
calculateAverages rows = 
    let transposed = transpose rows
    in mapM average transposed
  where
    transpose :: [[a]] -> [[a]]
    transpose ([]:_) = []
    transpose x = map head x : transpose (map tail x)
    
    average :: [Double] -> Maybe Double
    average xs = 
        if null xs 
        then Nothing 
        else Just (sum xs / fromIntegral (length xs))

processData :: String -> Maybe [Double]
processData csvContent = do
    dataset <- parseCSV csvContent
    calculateAverages dataset

sampleData :: String
sampleData = "1.0,2.0,3.0\n4.0,5.0,6.0\n7.0,8.0,9.0"