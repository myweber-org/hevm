module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input = 
    if null input
    then Left "Empty input"
    else Right $ map (splitOn ',') (lines input)
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitHelper [""]
      where
        splitHelper ch (x:xs)
            | ch == delimiter = "":x:xs
            | otherwise = (ch:x):xs

validateNumericColumn :: CSVData -> Int -> Either String CSVData
validateNumericColumn [] _ = Left "Empty data"
validateNumericColumn rows colIndex
    | colIndex < 0 = Left "Negative column index"
    | any (\row -> length row <= colIndex) rows = 
        Left $ "Column index " ++ show colIndex ++ " out of bounds"
    | not (all (all isDigit . dropWhile (== '-')) (map (!! colIndex) rows)) =
        Left $ "Column " ++ show colIndex ++ " contains non-numeric values"
    | otherwise = Right rows

calculateColumnSum :: CSVData -> Int -> Either String Double
calculateColumnSum rows colIndex = do
    validated <- validateNumericColumn rows colIndex
    let values = map (read . (!! colIndex)) validated
    return $ sum values

formatCSVOutput :: CSVData -> String
formatCSVOutput = intercalate "\n" . map (intercalate ",")

processCSVData :: String -> Int -> Either String (String, Double)
processCSVData rawData colIndex = do
    parsed <- parseCSV rawData
    sumResult <- calculateColumnSum parsed colIndex
    return (formatCSVOutput parsed, sumResult)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (* 2)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [1, -2, 3, -4, 5]
    let result = processNumbers numbers
    print result