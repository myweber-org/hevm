module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input = mapM parseRow (lines input)
  where
    parseRow line = case splitOnComma line of
        [] -> Left "Empty row"
        cells -> Right cells

splitOnComma :: String -> [String]
splitOnComma = foldr splitHelper [""]
  where
    splitHelper ',' (current:rest) = "":current:rest
    splitHelper char (current:rest) = (char:current):rest

validateNumericColumn :: CSVData -> Int -> Either String CSVData
validateNumericColumn [] _ = Right []
validateNumericColumn (row:rows) colIndex
    | colIndex < 0 = Left "Column index cannot be negative"
    | colIndex >= length row = Left "Column index out of bounds"
    | all isDigit (row !! colIndex) = do
        rest <- validateNumericColumn rows colIndex
        Right (row:rest)
    | otherwise = Left $ "Non-numeric value in column " ++ show colIndex

formatCSV :: CSVData -> String
formatCSV = intercalate "\n" . map (intercalate ",")

processCSVData :: String -> Int -> Either String String
processCSVData input colIndex = do
    parsed <- parseCSV input
    validated <- validateNumericColumn parsed colIndex
    return $ formatCSV validated
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateAndProcess :: [Int] -> Maybe [Int]
validateAndProcess xs
    | null xs = Nothing
    | otherwise = Just $ processData xs
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processEvenSquares

main :: IO ()
main = do
    let testData = [1..10]
    putStrLn $ "Original data: " ++ show testData
    putStrLn $ "Processed data: " ++ show (processEvenSquares testData)
    putStrLn $ "Sum of processed data: " ++ show (sumProcessedData testData)