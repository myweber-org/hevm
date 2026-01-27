module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (>0) (*2)

main :: IO ()
main = do
    let input = [1, -2, 3, 0, 5, -8]
    let result = processData input
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let numbers = [1..20]
    let result = processNumbers numbers
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

import Data.List (intercalate)
import Text.Read (readMaybe)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ',') (lines content)
  where
    splitOn :: Char -> String -> Row
    splitOn delimiter = foldr splitter [[]]
      where
        splitter char acc@(current:rest)
          | char == delimiter = []:acc
          | otherwise = (char:current):rest

validateNumericColumn :: CSVData -> Int -> Either String [Double]
validateNumericColumn rows columnIndex
  | null rows = Left "Empty CSV data"
  | columnIndex < 0 = Left "Column index cannot be negative"
  | otherwise = traverse validateRow rows
  where
    validateRow row
      | columnIndex >= length row = Left $ "Row has only " ++ show (length row) ++ " columns"
      | otherwise = case readMaybe (row !! columnIndex) of
          Just num -> Right num
          Nothing -> Left $ "Invalid numeric value: " ++ show (row !! columnIndex)

calculateColumnStats :: [Double] -> (Double, Double, Double)
calculateColumnStats values
  | null values = (0, 0, 0)
  | otherwise = (minimum values, maximum values, sum values / fromIntegral (length values))

processCSVFile :: String -> Int -> Either String String
processCSVFile content columnIndex = do
  let parsed = parseCSV content
  numericData <- validateNumericColumn parsed columnIndex
  let (minVal, maxVal, avgVal) = calculateColumnStats numericData
  return $ intercalate "\n"
    [ "Processed " ++ show (length numericData) ++ " rows",
      "Minimum: " ++ show minVal,
      "Maximum: " ++ show maxVal,
      "Average: " ++ show avgVal
    ]