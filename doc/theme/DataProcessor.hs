module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (*2)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show resultmodule DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input = 
    if null input
    then Left "Empty input"
    else Right $ map parseRow (lines input)
  where
    parseRow :: String -> CSVRow
    parseRow row = splitByComma row 0 [] ""
    
    splitByComma :: String -> Int -> [String] -> String -> [String]
    splitByComma [] _ acc current = reverse (reverse current : acc)
    splitByComma (c:cs) quoteDepth acc current
        | c == '"' = splitByComma cs (1 - quoteDepth) acc (c:current)
        | c == ',' && quoteDepth == 0 = splitByComma cs 0 (reverse current : acc) ""
        | otherwise = splitByComma cs quoteDepth acc (c:current)

validateNumericColumn :: CSVData -> Int -> Either String CSVData
validateNumericColumn [] _ = Right []
validateNumericColumn (row:rows) colIndex
    | colIndex < 0 || colIndex >= length row = Left $ "Column index " ++ show colIndex ++ " out of bounds"
    | all isNumeric (row !! colIndex) = do
        rest <- validateNumericColumn rows colIndex
        Right (row : rest)
    | otherwise = Left $ "Non-numeric value in column " ++ show colIndex ++ ": " ++ (row !! colIndex)
  where
    isNumeric :: String -> Bool
    isNumeric = all (\c -> isDigit c || c == '.')

calculateColumnAverage :: CSVData -> Int -> Either String Double
calculateColumnAverage [] _ = Left "Empty data"
calculateColumnAverage rows colIndex = do
    validated <- validateNumericColumn rows colIndex
    let values = map (read . (!! colIndex)) validated
    Right (sum values / fromIntegral (length values))

formatCSVOutput :: CSVData -> String
formatCSVOutput = intercalate "\n" . map (intercalate ",")

processCSVData :: String -> Int -> Either String (String, Double)
processCSVData csvContent column = do
    parsed <- parseCSV csvContent
    avg <- calculateColumnAverage parsed column
    let formatted = formatCSVOutput parsed
    Right (formatted, avg)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 10) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Maybe [Int]
validateInput xs
    | null xs = Nothing
    | any (< 0) xs = Nothing
    | otherwise = Just xs

safeProcess :: [Int] -> Maybe Int
safeProcess xs = do
    valid <- validateInput xs
    return $ sumProcessedData valid