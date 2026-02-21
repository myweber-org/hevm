module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isAlpha)
import Control.Monad (when)

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

validateId :: String -> Either ValidationError String
validateId idStr
    | all isDigit idStr && length idStr == 5 = Right idStr
    | otherwise = Left $ "Invalid ID format: " ++ idStr

validateName :: String -> Either ValidationError String
validateName name
    | all isAlpha name && length name >= 2 = Right name
    | otherwise = Left $ "Invalid name: " ++ name

validateAge :: String -> Either ValidationError String
validateAge ageStr = case reads ageStr :: [(Int, String)] of
    [(age, "")] -> if age >= 18 && age <= 120
                   then Right ageStr
                   else Left $ "Age out of range: " ++ ageStr
    _ -> Left $ "Invalid age format: " ++ ageStr

processCSVData :: [CSVRow] -> Either ValidationError [CSVRow]
processCSVData rows = mapM validateCSVRow rows

formatResults :: Either ValidationError [CSVRow] -> String
formatResults (Left err) = "Validation failed: " ++ err
formatResults (Right rows) = "Validated " ++ show (length rows) ++ " rows:\n" ++
    intercalate "\n" (map (intercalate ",") rows)module DataProcessor where

import Data.List (intercalate)
import Text.Read (readMaybe)

data ValidationError = InvalidRow Int String | InvalidColumn Int Int String
    deriving (Show, Eq)

type CSVRow = [String]
type CSVData = [CSVRow]

validateCSV :: CSVData -> Either [ValidationError] CSVData
validateCSV rows = case errors of
    [] -> Right rows
    _  -> Left errors
  where
    errors = concatMap validateRow (zip [1..] rows)
    
    validateRow (rowNum, row) = 
        concatMap (validateCell rowNum) (zip [1..] row)
    
    validateCell rowNum (colNum, cell)
        | null cell = [InvalidColumn rowNum colNum "Empty cell"]
        | not (all isValidChar cell) = 
            [InvalidColumn rowNum colNum "Contains invalid characters"]
        | otherwise = []
    
    isValidChar c = c `elem` (['a'..'z'] ++ ['A'..'z'] ++ ['0'..'9'] ++ " .,-")

parseNumericColumn :: CSVData -> Int -> Either String [Double]
parseNumericColumn rows colIndex
    | null rows = Left "Empty data"
    | colIndex < 1 || colIndex > length (head rows) = 
        Left $ "Column index " ++ show colIndex ++ " out of bounds"
    | otherwise = 
        case traverse parseRow rows of
            Right nums -> Right nums
            Left err -> Left err
  where
    parseRow row = 
        case readMaybe (row !! (colIndex - 1)) of
            Just num -> Right num
            Nothing -> Left $ "Invalid number in column " ++ show colIndex

calculateStatistics :: [Double] -> (Double, Double, Double)
calculateStatistics nums
    | null nums = (0, 0, 0)
    | otherwise = (mean, minimum nums, maximum nums)
  where
    mean = sum nums / fromIntegral (length nums)

processCSVData :: CSVData -> Int -> Either String (Double, Double, Double)
processCSVData rows colIndex = do
    validated <- case validateCSV rows of
        Right data' -> Right data'
        Left errs -> Left $ "Validation errors: " ++ show errs
    
    numericData <- parseNumericColumn validated colIndex
    
    if null numericData
        then Left "No numeric data available"
        else Right $ calculateStatistics numericData

formatErrors :: [ValidationError] -> String
formatErrors errs = intercalate "\n" $ map formatError errs
  where
    formatError (InvalidRow row msg) = 
        "Row " ++ show row ++ ": " ++ msg
    formatError (InvalidColumn row col msg) = 
        "Cell (" ++ show row ++ "," ++ show col ++ "): " ++ msgmodule DataProcessor where

import Data.List (tails)

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | n > length xs = []
    | otherwise = map avg $ filter (\window -> length window == n) $ tails xs
  where
    avg window = sum window / fromIntegral n

-- Example usage:
-- movingAverage 3 [1,2,3,4,5] -> [2.0,3.0,4.0]module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

import Data.List (foldl')
import Data.Maybe (catMaybes)

type Record = [String]
type CSVData = [Record]

parseCSV :: String -> CSVData
parseCSV = map (splitOn ',') . lines
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitHelper [""]
      where
        splitHelper ch (x:xs)
          | ch == delimiter = "":x:xs
          | otherwise = (ch:x):xs

safeReadDouble :: String -> Maybe Double
safeReadDouble s = case reads s of
  [(val, "")] -> Just val
  _ -> Nothing

computeColumnStats :: CSVData -> Int -> Maybe (Double, Double, Double)
computeColumnStats csvData colIndex
  | null csvData = Nothing
  | otherwise = do
      let values = catMaybes $ map (safeReadDouble . (!! colIndex)) csvData
      guard (not (null values))
      let sumVal = foldl' (+) 0 values
      let count = fromIntegral $ length values
      let avg = sumVal / count
      let maxVal = maximum values
      let minVal = minimum values
      return (avg, maxVal, minVal)

filterByColumn :: (String -> Bool) -> Int -> CSVData -> CSVData
filterByColumn predicate colIndex = 
  filter (\record -> 
    colIndex < length record && predicate (record !! colIndex))

validateCSV :: CSVData -> Bool
validateCSV [] = True
validateCSV (firstRow:rows) = 
  let expectedCols = length firstRow
  in all (\row -> length row == expectedCols) rows

processCSVFile :: FilePath -> IO ()
processCSVFile filePath = do
  content <- readFile filePath
  let csvData = parseCSV content
  
  if validateCSV csvData
    then do
      putStrLn "CSV validation passed"
      case computeColumnStats csvData 0 of
        Just (avg, maxVal, minVal) -> do
          putStrLn $ "Average: " ++ show avg
          putStrLn $ "Maximum: " ++ show maxVal
          putStrLn $ "Minimum: " ++ show minVal
        Nothing -> putStrLn "Could not compute statistics for column 0"
    else putStrLn "Invalid CSV format: inconsistent column count"module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)
module DataProcessor where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.Vector (Vector)
import qualified Data.Vector as V

type Row = (String, Double, Double, Double)

parseCSV :: BL.ByteString -> Either String (Vector Row)
parseCSV input = case Csv.decode Csv.NoHeader input of
    Left err -> Left $ "Parse error: " ++ err
    Right rows -> Right $ V.fromList rows

calculateAverages :: Vector Row -> (Double, Double, Double)
calculateAverages rows
    | V.null rows = (0, 0, 0)
    | otherwise = (avg col1, avg col2, avg col3)
  where
    col1 = V.map (\(_, x, _, _) -> x) rows
    col2 = V.map (\(_, _, x, _) -> x) rows
    col3 = V.map (\(_, _, _, x) -> x) rows
    avg xs = V.sum xs / fromIntegral (V.length xs)

processData :: BL.ByteString -> Either String (Double, Double, Double)
processData input = do
    parsed <- parseCSV input
    return $ calculateAverages parsed