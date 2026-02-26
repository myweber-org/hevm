module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = 
    if all (> -100) xs && length xs <= 1000
    then Just xs
    else Nothing

safeProcess :: [Int] -> Maybe Int
safeProcess = fmap sumProcessed . validateInputmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print resultmodule DataProcessor where

import Data.List (intercalate)
import Text.Read (readMaybe)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ',') (lines content)
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitter [[]]
      where
        splitter char acc@(current:rest)
          | char == delimiter = []:acc
          | otherwise = (char:current):rest

validateNumericColumn :: CSVData -> Int -> Either String [Double]
validateNumericColumn rows columnIndex
  | null rows = Left "Empty CSV data"
  | columnIndex < 0 = Left "Invalid column index"
  | otherwise = traverse parseRow rows
  where
    parseRow row
      | columnIndex >= length row = Left "Column index out of bounds"
      | otherwise = case readMaybe (row !! columnIndex) of
          Just value -> Right value
          Nothing -> Left $ "Invalid numeric value in column " ++ show columnIndex

calculateColumnStats :: [Double] -> (Double, Double, Double)
calculateColumnStats values
  | null values = (0, 0, 0)
  | otherwise = (minimum values, maximum values, sum values / fromIntegral (length values))

processCSVFile :: String -> Int -> Either String String
processCSVFile content columnIndex = do
  let csvData = parseCSV content
  numericValues <- validateNumericColumn csvData columnIndex
  let (minVal, maxVal, avgVal) = calculateColumnStats numericValues
  return $ intercalate "\n"
    [ "Processed " ++ show (length numericValues) ++ " rows",
      "Minimum: " ++ show minVal,
      "Maximum: " ++ show maxVal,
      "Average: " ++ show avgVal
    ]
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData = all (> 0) . processData