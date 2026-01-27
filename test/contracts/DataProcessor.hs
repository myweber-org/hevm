
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)module DataProcessor where

import Data.List (transpose)
import Text.Read (readMaybe)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> Either String CSVData
parseCSV content = case lines content of
    [] -> Left "Empty file"
    rows -> let parsedRows = map (splitOn ',') rows
            in if allEqualLength parsedRows
                then Right parsedRows
                else Left "Rows have inconsistent column counts"
  where
    splitOn :: Char -> String -> Row
    splitOn delimiter = foldr splitHelper [[]]
      where
        splitHelper ch (current:rest)
            | ch == delimiter = []:current:rest
            | otherwise = (ch:current):rest

allEqualLength :: [Row] -> Bool
allEqualLength [] = True
allEqualLength (x:xs) = all ((== length x) . length) xs

validateNumericColumns :: CSVData -> [Int] -> Either String CSVData
validateNumericColumns dataRows columnIndices =
    case invalidColumns of
        [] -> Right dataRows
        _ -> Left $ "Non-numeric values found in columns: " ++ show invalidColumns
  where
    invalidColumns = filter (not . isColumnNumeric) columnIndices
    isColumnNumeric idx = all (isNumeric . (!! idx)) dataRows
    isNumeric str = case readMaybe str :: Maybe Double of
        Just _ -> True
        Nothing -> False

calculateColumnAverages :: CSVData -> [Int] -> Either String [Double]
calculateColumnAverages dataRows columnIndices
    | null dataRows = Left "No data rows"
    | any (>= length (head dataRows)) columnIndices = Left "Column index out of bounds"
    | otherwise = Right $ map calculateAverage columnIndices
  where
    calculateAverage idx = 
        let values = map (!! idx) dataRows
            numericValues = mapMaybe (readMaybe :: String -> Maybe Double) values
        in if null numericValues 
            then 0.0 
            else sum numericValues / fromIntegral (length numericValues)

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) = case f x of
    Just y -> y : mapMaybe f xs
    Nothing -> mapMaybe f xs
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Maybe [Int]
validateInput xs
    | null xs = Nothing
    | any (< -100) xs = Nothing
    | any (> 100) xs = Nothing
    | otherwise = Just xs

safeDataProcessing :: [Int] -> Maybe [Int]
safeDataProcessing input = do
    validated <- validateInput input
    return $ processData validated
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

main :: IO ()
main = do
    let numbers = [1, -2, 3, -4, 5]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)
    putStrLn $ "First element: " ++ show (safeHead numbers)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-5, 3, 0, 8, -2, 10]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -100) xs then Just xs else Nothingmodule DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | length xs < n = []
    | otherwise = avg : movingAverage n (tail xs)
  where
    window = take n xs
    avg = sum window / fromIntegral n

smoothData :: Fractional a => Int -> [a] -> [a]
smoothData windowSize dataPoints =
    let prefix = replicate (windowSize `div` 2) (head dataPoints)
        suffix = replicate (windowSize `div` 2) (last dataPoints)
        extendedData = prefix ++ dataPoints ++ suffix
    in movingAverage windowSize extendedData

calculateTrend :: Fractional a => [a] -> a
calculateTrend values =
    let n = fromIntegral (length values)
        indices = [0..n-1]
        sumX = sum indices
        sumY = sum values
        sumXY = sum (zipWith (*) indices values)
        sumX2 = sum (map (^2) indices)
        slope = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX)
    in slopemodule DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV content = 
    if null content
    then Left "Empty CSV content"
    else Right $ map parseRow (lines content)
  where
    parseRow line = splitByComma line
    splitByComma = foldr splitter [[]]
    splitter ',' (x:xs) = []:x:xs
    splitter c (x:xs) = (c:x):xs

validateNumericColumn :: CSVData -> Int -> Either String CSVData
validateNumericColumn [] _ = Left "Empty data"
validateNumericColumn rows colIndex
    | colIndex < 0 = Left "Column index cannot be negative"
    | any (\row -> length row <= colIndex) rows = 
        Left $ "Column index " ++ show colIndex ++ " out of bounds"
    | not (all (all isDigit . concat) (map (!! colIndex) rows)) =
        Left $ "Column " ++ show colIndex ++ " contains non-numeric values"
    | otherwise = Right rows

formatCSV :: CSVData -> String
formatCSV = intercalate "\n" . map (intercalate ",")

processCSVData :: String -> Int -> Either String String
processCSVData rawData columnIndex = do
    parsed <- parseCSV rawData
    validated <- validateNumericColumn parsed columnIndex
    return $ formatCSV validated

sampleData :: String
sampleData = "id,name,age\n1,John,25\n2,Jane,30\n3,Bob,35"