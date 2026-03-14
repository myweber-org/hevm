module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

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
processNumbers = filterAndTransform even (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just xmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -1000) xs then Just xs else Nothingmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)
module DataProcessor where

import Data.Time
import Data.Time.Format
import Data.List
import Data.Char

parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

data Record = Record
    { recordDate :: Day
    , recordValue :: Double
    , recordCategory :: String
    } deriving (Show, Eq)

parseRecord :: String -> Maybe Record
parseRecord line = case words line of
    [dateStr, valueStr, category] -> do
        date <- parseDate dateStr
        value <- readMaybe valueStr
        return Record { recordDate = date, recordValue = value, recordCategory = category }
    _ -> Nothing
  where
    readMaybe :: String -> Maybe Double
    readMaybe s = case reads s of
        [(x, "")] -> Just x
        _ -> Nothing

filterByDateRange :: Day -> Day -> [Record] -> [Record]
filterByDateRange start end = filter (\r -> recordDate r >= start && recordDate r <= end)

summarizeByCategory :: [Record] -> [(String, Double)]
summarizeByCategory records =
    let grouped = groupBy (\a b -> recordCategory a == recordCategory b) $
                  sortOn recordCategory records
    in map (\group -> (recordCategory (head group), sum (map recordValue group))) grouped

processData :: String -> Day -> Day -> Either String [(String, Double)]
processData content start end = do
    let lines' = filter (not . all isSpace) (lines content)
    records <- mapM parseRecord lines'
    let filtered = filterByDateRange start end records
    return $ summarizeByCategory filteredmodule DataProcessor where

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
import Data.Char (isDigit, isAlpha)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input
    | null input = Right []
    | otherwise = mapM parseRow (lines input)
  where
    parseRow line = case splitOnComma line of
        [] -> Left "Empty row"
        cells -> Right cells

splitOnComma :: String -> [String]
splitOnComma = foldr splitHelper [""]
  where
    splitHelper ',' (current:rest) = "":current:rest
    splitHelper char (current:rest) = (char:current):rest

validateRow :: CSVRow -> Either String CSVRow
validateRow row = do
    when (length row < 2) $ Left "Row must have at least 2 columns"
    validateId (head row)
    validateName (row !! 1)
    return row

validateId :: String -> Either String String
validateId idStr
    | all isDigit idStr && not (null idStr) = Right idStr
    | otherwise = Left $ "Invalid ID: " ++ idStr

validateName :: String -> Either String String
validateName name
    | all isAlpha name && not (null name) = Right name
    | otherwise = Left $ "Invalid name: " ++ name

processCSVData :: String -> Either String CSVData
processCSVData input = do
    parsed <- parseCSV input
    mapM validateRow parsed

formatOutput :: CSVData -> String
formatOutput rows = intercalate "\n" $ map formatRow rows
  where
    formatRow cells = intercalate "," $ map escapeComma cells
    escapeComma cell
        | ',' `elem` cell = "\"" ++ cell ++ "\""
        | otherwise = cell

main :: IO ()
main = do
    let testData = "123,John\n456,Alice Smith\n789,Bob"
    case processCSVData testData of
        Left err -> putStrLn $ "Error: " ++ err
        Right data' -> putStrLn $ "Processed data:\n" ++ formatOutput data'module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

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
    let ma5 = movingAverage 5 dataSeries
    putStrLn $ "Original series: " ++ show dataSeries
    putStrLn $ "3-period moving average: " ++ show ma3
    putStrLn $ "5-period moving average: " ++ show ma5module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbers

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just xmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = not (null xs) && all (>= -100) xs && all (<= 100) xs

safeProcess :: [Int] -> Maybe [Int]
safeProcess xs
    | validateInput xs = Just (processData xs)
    | otherwise = Nothing
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Bool
validateInput = all (\x -> x >= -100 && x <= 100)

safeProcessData :: [Int] -> Maybe [Int]
safeProcessData xs
    | validateInput xs = Just (processData xs)
    | otherwise = Nothingmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print result
module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type CSVRow = [String]
type ValidationError = String

validateRow :: CSVRow -> Either ValidationError CSVRow
validateRow [] = Left "Empty row"
validateRow row
    | length row < 3 = Left "Row must have at least 3 columns"
    | not (all validField row) = Left "All fields must be non-empty"
    | not (validAge (row !! 1)) = Left "Second column must be a valid age"
    | otherwise = Right row
  where
    validField field = not (null field) && not (any (==',') field)
    validAge ageStr = all isDigit ageStr && not (null ageStr)

parseCSV :: String -> Either ValidationError [CSVRow]
parseCSV content = 
    let rows = map (splitOn ',') (lines content)
        validatedRows = map validateRow rows
    in sequence validatedRows

splitOn :: Char -> String -> [String]
splitOn delimiter = foldr splitHelper [""]
  where
    splitHelper char acc@(current:rest)
        | char == delimiter = "":acc
        | otherwise = (char:current):rest

formatOutput :: [CSVRow] -> String
formatOutput rows = 
    let formattedRows = map (intercalate " | ") rows
    in unlines formattedRows

processCSVData :: String -> Either ValidationError String
processCSVData input = do
    parsed <- parseCSV input
    return $ formatOutput parsedmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = 
    filterAndTransform (> 10) (\x -> x * 2 + 1)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs
    | null xs = Nothing
    | any (< 0) xs = Nothing
    | otherwise = Just xs

safeProcess :: [Int] -> Maybe Int
safeProcess = fmap sumProcessed . validateInputmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, 0, 5, -8]
    let result = processNumbers input
    putStrLn $ "Input list: " ++ show input
    putStrLn $ "Filtered and transformed: " ++ show resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (*2)