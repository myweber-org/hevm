module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, 0, 5, -7]
    let result = processNumbers input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show resultmodule DataProcessor where

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
import Data.Char (isDigit)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> Either String CSVData
parseCSV input = 
    if null input
    then Left "Empty input"
    else Right $ map (splitOn ',') (lines input)

splitOn :: Char -> String -> [String]
splitOn delimiter = foldr splitHelper [""]
  where
    splitHelper char acc@(current:rest)
        | char == delimiter = "":acc
        | otherwise = (char:current):rest

validateNumericColumn :: CSVData -> Int -> Either String CSVData
validateNumericColumn [] _ = Left "Empty data"
validateNumericColumn rows columnIndex
    | columnIndex < 0 = Left "Negative column index"
    | any (\row -> columnIndex >= length row) rows = Left "Column index out of bounds"
    | not (all (isNumeric . (!! columnIndex)) rows) = Left "Non-numeric values found"
    | otherwise = Right rows
  where
    isNumeric str = not (null str) && all isDigit str

calculateColumnAverage :: CSVData -> Int -> Either String Double
calculateColumnAverage rows columnIndex = do
    validated <- validateNumericColumn rows columnIndex
    let values = map (read . (!! columnIndex)) validated
    return (sum values / fromIntegral (length values))

formatCSVOutput :: CSVData -> String
formatCSVOutput = intercalate "\n" . map (intercalate ",")

processCSVFile :: String -> Int -> Either String (String, Double)
processCSVFile content columnIndex = do
    parsed <- parseCSV content
    average <- calculateColumnAverage parsed columnIndex
    return (formatCSVOutput parsed, average)