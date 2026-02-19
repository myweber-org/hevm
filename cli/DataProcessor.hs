module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input = case lines input of
    [] -> Left "Empty input"
    rows -> traverse parseRow rows
  where
    parseRow :: String -> Either String CSVRow
    parseRow row = case splitByComma row of
        [] -> Left "Empty row"
        cells -> Right cells

    splitByComma :: String -> [String]
    splitByComma = foldr splitter [""]
      where
        splitter ',' (current:rest) = "":current:rest
        splitter char (current:rest) = (char:current):rest

validateNumericColumn :: CSVData -> Int -> Either String CSVData
validateNumericColumn [] _ = Left "No data to validate"
validateNumericColumn rows colIndex
    | colIndex < 0 = Left "Column index cannot be negative"
    | otherwise = traverse validateRow rows
  where
    validateRow :: CSVRow -> Either String CSVRow
    validateRow row
        | colIndex >= length row = Left $ "Row has only " ++ show (length row) ++ " columns"
        | all isDigit (row !! colIndex) = Right row
        | otherwise = Left $ "Non-numeric value in column " ++ show colIndex ++ ": " ++ (row !! colIndex)

formatCSV :: CSVData -> String
formatCSV = intercalate "\n" . map (intercalate ",")

processCSVData :: String -> Int -> Either String String
processCSVData input colIndex = do
    parsed <- parseCSV input
    validated <- validateNumericColumn parsed colIndex
    return $ formatCSV validatedmodule DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | length xs < n = []
    | otherwise = avg : movingAverage n (tail xs)
  where
    window = take n xs
    avg = sum window / fromIntegral n

-- Example usage with a helper function
exampleUsage :: IO ()
exampleUsage = do
    let dataSeries = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
    let ma3 = movingAverage 3 dataSeries
    putStrLn $ "Original series: " ++ show dataSeries
    putStrLn $ "3-period moving average: " ++ show ma3