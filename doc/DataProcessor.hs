
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData = all (> 0)

main :: IO ()
main = do
    let inputData = [1, -2, 3, 0, 5, -7]
    let processed = processData inputData
    putStrLn $ "Original data: " ++ show inputData
    putStrLn $ "Processed data: " ++ show processed
    putStrLn $ "Data valid: " ++ show (validateData processed)
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
    len = fromIntegral $ V.length rows
    col1 = V.map (\(_, a, _, _) -> a) rows
    col2 = V.map (\(_, _, b, _) -> b) rows
    col3 = V.map (\(_, _, _, c) -> c) rows
    avg vec = V.sum vec / len

processData :: BL.ByteString -> Either String (Double, Double, Double)
processData input = do
    parsed <- parseCSV input
    return $ calculateAverages parsedmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [1, -2, 3, 0, 5, -8]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)
module DataProcessor where

import Data.List (foldl')
import Data.List.Split (splitOn)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ",") (lines content)

safeReadDouble :: String -> Maybe Double
safeReadDouble s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing

computeColumnStats :: CSVData -> Int -> Maybe (Double, Double, Double)
computeColumnStats [] _ = Nothing
computeColumnStats (header:rows) colIndex
    | colIndex < 0 || colIndex >= length header = Nothing
    | otherwise = processRows rows
  where
    processRows [] = Nothing
    processRows rs = case mapM (safeReadDouble . (!! colIndex)) rs of
        Nothing -> Nothing
        Just values -> let
            count = fromIntegral (length values)
            sumVal = foldl' (+) 0 values
            avg = sumVal / count
            minVal = minimum values
            maxVal = maximum values
            in Just (avg, minVal, maxVal)

filterRowsByColumn :: CSVData -> Int -> (Double -> Bool) -> CSVData
filterRowsByColumn [] _ _ = []
filterRowsByColumn (header:rows) colIndex predicate =
    header : filter (rowPredicate predicate) rows
  where
    rowPredicate pred row
        | colIndex >= length row = False
        | otherwise = case safeReadDouble (row !! colIndex) of
            Just val -> pred val
            Nothing -> False

formatStats :: Maybe (Double, Double, Double) -> String
formatStats Nothing = "Invalid column index or data"
formatStats (Just (avg, minVal, maxVal)) =
    "Average: " ++ show avg ++
    ", Min: " ++ show minVal ++
    ", Max: " ++ show maxValmodule DataProcessor where

import Data.List (transpose)
import Data.Maybe (isJust, fromJust)

type CSV = [[String]]

parseCSV :: String -> Maybe CSV
parseCSV content = if allRowsValid rows then Just rows else Nothing
  where
    rows = map (splitOn ',') (lines content)
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr (\c acc -> if c == delimiter then []:acc else (c:head acc):tail acc) [[]]
    allRowsValid :: [[String]] -> Bool
    allRowsValid [] = True
    allRowsValid (x:xs) = all (\row -> length row == length x) xs

validateCSV :: CSV -> Either String CSV
validateCSV [] = Left "CSV data is empty"
validateCSV rows@(firstRow:_)
  | any null rows = Left "CSV contains empty rows"
  | not (allEqualLength rows) = Left "Rows have inconsistent column counts"
  | otherwise = Right rows
  where
    allEqualLength :: [[a]] -> Bool
    allEqualLength [] = True
    allEqualLength (x:xs) = all (\row -> length row == length x) xs

getColumn :: Int -> CSV -> Maybe [String]
getColumn index csv
  | null csv = Nothing
  | index < 0 || index >= length (head csv) = Nothing
  | otherwise = Just (map (!! index) csv)

transposeCSV :: CSV -> CSV
transposeCSV = transpose

countRows :: CSV -> Int
countRows = length

countColumns :: CSV -> Int
countColumns [] = 0
countColumns (row:_) = length rowmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 10) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, 0, 5, -7]
    let result = processNumbers input
    print result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processEvenSquares

main :: IO ()
main = do
    let sampleData = [1..10]
    putStrLn $ "Original data: " ++ show sampleData
    putStrLn $ "Processed data: " ++ show (processEvenSquares sampleData)
    putStrLn $ "Sum of processed data: " ++ show (sumProcessedData sampleData)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isAlpha)

type Row = [String]
type CSV = [Row]

validateRow :: Row -> Bool
validateRow row = length row == 3 && all validField row
  where
    validField field = not (null field) && length field <= 50

transformPhone :: String -> String
transformPhone phone
  | all isDigit phone && length phone == 10 = 
      "(" ++ take 3 phone ++ ") " ++ drop 3 (take 6 phone) ++ "-" ++ drop 6 phone
  | otherwise = phone

normalizeName :: String -> String
normalizeName = unwords . map capitalize . words
  where
    capitalize [] = []
    capitalize (x:xs) = toUpper x : map toLower xs
    toUpper = toEnum . subtract 32 . fromEnum
    toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

processCSV :: CSV -> Either String CSV
processCSV [] = Left "Empty CSV data"
processCSV (header:rows)
  | length header /= 3 = Left "Invalid header format"
  | otherwise = 
      case filter (not . validateRow) rows of
        [] -> Right $ header : map processRow rows
        invalidRows -> Left $ "Invalid rows at positions: " ++ show (findInvalidPositions rows invalidRows)
  where
    processRow [name, email, phone] = 
      [normalizeName name, email, transformPhone phone]
    processRow row = row
    
    findInvalidPositions allRows invalidRows =
      [i | (i, row) <- zip [1..] allRows, row `elem` invalidRows]

formatCSV :: CSV -> String
formatCSV = intercalate "\n" . map (intercalate ",")

sampleData :: CSV
sampleData =
  [ ["Name", "Email", "Phone"]
  , ["john doe", "john@example.com", "1234567890"]
  , ["JANE SMITH", "jane@test.org", "555-1234"]
  , ["", "invalid@test.com", "9876543210"]
  ]module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processData

main :: IO ()
main = do
    let testData = [-3, 2, 0, 5, -1, 8]
    putStrLn $ "Original data: " ++ show testData
    putStrLn $ "Processed data: " ++ show (processData testData)
    putStrLn $ "Sum of positive doubles: " ++ show (sumPositiveDoubles testData)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = not (null xs) && all (>= -100) xs && all (<= 100) xs

main :: IO ()
main = do
    let sampleData = [1, -2, 3, 0, 5, -8]
    if validateInput sampleData
        then print $ processData sampleData
        else putStrLn "Invalid input data"