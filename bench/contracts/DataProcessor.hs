module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isAlpha)
import Control.Monad (when)
import System.IO (hPutStrLn, stderr)

type CSVRow = [String]
type ValidationError = String

validateCSVRow :: CSVRow -> Either ValidationError CSVRow
validateCSVRow [] = Left "Empty row"
validateCSVRow row = do
    when (length row < 3) $ Left "Row must have at least 3 columns"
    validateId (head row)
    validateName (row !! 1)
    validateAge (row !! 2)
    return row
  where
    validateId :: String -> Either ValidationError String
    validateId idStr
        | all isDigit idStr && length idStr == 6 = Right idStr
        | otherwise = Left $ "Invalid ID format: " ++ idStr

    validateName :: String -> Either ValidationError String
    validateName name
        | all isAlpha name && length name >= 2 = Right name
        | otherwise = Left $ "Invalid name format: " ++ name

    validateAge :: String -> Either ValidationError String
    validateAge ageStr = case reads ageStr of
        [(age, "")] | age >= 0 && age <= 150 -> Right ageStr
        _ -> Left $ "Invalid age value: " ++ ageStr

processCSVData :: [CSVRow] -> IO ()
processCSVData rows = do
    let results = map validateCSVRow rows
    mapM_ (either handleError logSuccess) (zip [1..] results)
  where
    handleError (rowNum, err) = 
        hPutStrLn stderr $ "Row " ++ show rowNum ++ " error: " ++ err
    logSuccess (rowNum, row) = 
        putStrLn $ "Row " ++ show rowNum ++ " valid: " ++ intercalate "," row

sampleData :: [CSVRow]
sampleData =
    [ ["123456", "Alice", "30"]
    , ["789012", "Bob", "25"]
    , ["invalid", "C", "200"]
    , ["654321", "David2", "40"]
    , ["987654", "Eve", "-5"]
    ]

main :: IO ()
main = processCSVData sampleDatamodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -1000) xs then Just xs else Nothing
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedList :: [Int] -> Int
sumProcessedList = sum . processEvenSquares

main :: IO ()
main = do
    let sampleData = [1..10]
    putStrLn $ "Original list: " ++ show sampleData
    putStrLn $ "Processed list: " ++ show (processEvenSquares sampleData)
    putStrLn $ "Sum of processed list: " ++ show (sumProcessedList sampleData)
module DataProcessor where

import Data.List (intercalate)
import Text.Read (readMaybe)

type Row = [String]
type Table = [Row]

parseCSV :: String -> Table
parseCSV content = map (splitOn ',') (lines content)
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitter [[]]
      where
        splitter char (current:rest)
          | char == delimiter = []:current:rest
          | otherwise = (char:current):rest

filterRows :: (Row -> Bool) -> Table -> Table
filterRows predicate = filter predicate

numericGreaterThan :: Row -> Int -> Double -> Bool
numericGreaterThan row columnIndex threshold =
  case getNumericValue row columnIndex of
    Just value -> value > threshold
    Nothing -> False

getNumericValue :: Row -> Int -> Maybe Double
getNumericValue row idx
  | idx < 0 || idx >= length row = Nothing
  | otherwise = readMaybe (row !! idx)

formatTable :: Table -> String
formatTable = intercalate "\n" . map (intercalate ",")

processData :: String -> Int -> Double -> String
processData csvContent columnIndex threshold =
  let table = parseCSV csvContent
      filtered = filterRows (\row -> numericGreaterThan row columnIndex threshold) table
  in formatTable filtered