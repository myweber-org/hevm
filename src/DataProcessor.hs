
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
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processDatamodule DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isSpace)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input
    | null (trim input) = Right []
    | otherwise = mapM parseRow (lines input)
  where
    parseRow :: String -> Either String CSVRow
    parseRow line = traverse validateField (splitOnComma line)
    
    splitOnComma :: String -> [String]
    splitOnComma = foldr splitHelper [""]
      where
        splitHelper ',' acc = "":acc
        splitHelper ch (x:xs) = (ch:x):xs
        splitHelper _ [] = error "Impossible pattern"
    
    validateField :: String -> Either String String
    validateField field
        | any isInvalidChar field = Left $ "Invalid character in field: " ++ show field
        | length field > 100 = Left $ "Field too long: " ++ show field
        | otherwise = Right (trim field)
    
    isInvalidChar :: Char -> Bool
    isInvalidChar c = c < ' ' && c /= '\t'
    
    trim :: String -> String
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

validateNumericFields :: CSVData -> Either String CSVData
validateNumericFields [] = Right []
validateNumericData rows = do
    validatedRows <- mapM validateRow rows
    return validatedRows
  where
    validateRow :: CSVRow -> Either String CSVRow
    validateRow [] = Right []
    validateRow (field:fields) = do
        validatedField <- if all isDigit (filter (/= '.') field)
                         then Right field
                         else Left $ "Non-numeric value in numeric field: " ++ field
        rest <- validateRow fields
        return (validatedField:rest)

processCSVData :: String -> Either String CSVData
processCSVData input = do
    parsedData <- parseCSV input
    validatedData <- validateNumericFields parsedData
    return validatedData

formatCSVOutput :: CSVData -> String
formatCSVOutput = intercalate "\n" . map (intercalate ",")

sampleData :: String
sampleData = "123,John Doe,45.6\n456,Jane Smith,78.9\n789,Bob Johnson,abc"

main :: IO ()
main = do
    putStrLn "Processing CSV data..."
    case processCSVData sampleData of
        Left err -> putStrLn $ "Error: " ++ err
        Right data -> do
            putStrLn "Processed successfully:"
            putStrLn $ formatCSVOutput data