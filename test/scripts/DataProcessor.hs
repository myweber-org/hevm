module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if null xs then Nothing else Just xsmodule DataProcessor where

import Data.List (tails)

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map avg $ filter (\window -> length window == n) $ tails xs
  where
    avg window = sum window / fromIntegral n

smoothData :: Fractional a => Int -> [a] -> [a]
smoothData windowSize dataPoints =
    movingAverage windowSize dataPoints

calculateTrend :: Fractional a => [a] -> a
calculateTrend values =
    let n = fromIntegral $ length values
        indices = [0..n-1]
        sumX = sum indices
        sumY = sum values
        sumXY = sum $ zipWith (*) indices values
        sumX2 = sum $ map (^2) indices
        slope = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX)
    in slope
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessed :: (Int -> Int) -> [Int] -> Int
sumProcessed processor = sum . map processor

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Even squares: " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of even squares: " ++ 
        show (sumProcessed (\x -> x * x) (filter even numbers))
module DataProcessor where

import Data.List (foldl')
import Text.Read (readMaybe)

type Record = (String, Double, Double)

parseCSVLine :: String -> Maybe Record
parseCSVLine line = case wordsWhen (==',') line of
    [name, val1, val2] -> do
        v1 <- readMaybe val1
        v2 <- readMaybe val2
        return (name, v1, v2)
    _ -> Nothing

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

calculateAverages :: [Record] -> (Double, Double)
calculateAverages records = 
    let (sum1, sum2, count) = foldl' (\(s1, s2, c) (_, v1, v2) -> (s1 + v1, s2 + v2, c + 1)) (0, 0, 0) records
    in if count > 0 
        then (sum1 / fromIntegral count, sum2 / fromIntegral count)
        else (0, 0)

processCSVData :: String -> Maybe ((Double, Double), [Record])
processCSVData csvContent = do
    let lines' = filter (not . null) $ lines csvContent
    records <- mapM parseCSVLine lines'
    if null records 
        then Nothing
        else Just (calculateAverages records, records)

formatOutput :: ((Double, Double), [Record]) -> String
formatOutput ((avg1, avg2), records) =
    "Processed " ++ show (length records) ++ " records\n" ++
    "Average value 1: " ++ show avg1 ++ "\n" ++
    "Average value 2: " ++ show avg2 ++ "\n" ++
    "Individual records:\n" ++
    unlines (map (\(name, v1, v2) -> name ++ ": " ++ show v1 ++ ", " ++ show v2) records)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processEvenSquares

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list (even numbers squared): " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of processed numbers: " ++ show (sumProcessed numbers)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData xs = all (> 0) (processData xs)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

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

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -100) xs then Just xs else Nothing
module DataProcessor where

import Data.Time
import Text.CSV

filterCSVByDate :: String -> Day -> Day -> Either String [(String, Day, Double)]
filterCSVByDate csvContent startDate endDate = do
    csv <- parseCSV "input" csvContent
    let filtered = filter (\(_, dateStr, _) -> 
            case parseTimeM True defaultTimeLocale "%Y-%m-%d" dateStr of
                Just date -> date >= startDate && date <= endDate
                Nothing -> False) (processRows csv)
    return filtered
  where
    processRows :: CSV -> [(String, Day, Double)]
    processRows [] = []
    processRows (row:rows) = 
        case row of
            [name, dateStr, valueStr] -> 
                case (parseTimeM True defaultTimeLocale "%Y-%m-%d" dateStr, reads valueStr) of
                    (Just date, [(value, "")]) -> (name, date, value) : processRows rows
                    _ -> processRows rows
            _ -> processRows rows
module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type CSVRow = [String]
type ValidationError = String

validateRow :: Int -> CSVRow -> Either ValidationError CSVRow
validateRow rowNum row
    | length row /= 3 = Left $ "Row " ++ show rowNum ++ ": Expected 3 columns, got " ++ show (length row)
    | not (all numeric [row !! 0, row !! 2]) = Left $ "Row " ++ show rowNum ++ ": Non-numeric values in numeric columns"
    | not (validName (row !! 1)) = Left $ "Row " ++ show rowNum ++ ": Invalid name format"
    | otherwise = Right row
  where
    numeric str = not (null str) && all isDigit str
    validName name = length name >= 2 && length name <= 50 && all (\c -> c == ' ' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) name

parseCSV :: String -> Either ValidationError [CSVRow]
parseCSV content = 
    let rows = map (splitOn ',') (lines content)
        validated = zipWith validateRow [1..] rows
    in sequence validated
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr f [[]]
      where
        f c (x:xs) | c == delimiter = []:x:xs
                   | otherwise = (c:x):xs

calculateTotals :: [CSVRow] -> (Int, Double)
calculateTotals rows = 
    foldl (\(count, total) row -> 
        let id = read (row !! 0) :: Int
            amount = read (row !! 2) :: Double
        in (count + 1, total + amount)) (0, 0.0) rows

processCSVData :: String -> Either ValidationError String
processCSVData input = do
    rows <- parseCSV input
    let (count, total) = calculateTotals rows
    return $ "Processed " ++ show count ++ " rows with total amount: " ++ show total

formatReport :: [CSVRow] -> String
formatReport rows = 
    let header = "ID,Name,Amount"
        separator = replicate 30 '-'
        formattedRows = map (intercalate ",") rows
    in unlines $ header : separator : formattedRowsmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Bool
validateInput = all (\x -> x >= -100 && x <= 100)

safeProcess :: [Int] -> Maybe Int
safeProcess xs
    | validateInput xs = Just (sumProcessedData xs)
    | otherwise = Nothing
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (\x -> x * 2 + 1)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

main :: IO ()
main = do
    let numbers = [1..20]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed numbers: " ++ show (sumProcessed numbers)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = all (\x -> x >= -100 && x <= 100) xs

safeProcess :: [Int] -> Maybe [Int]
safeProcess xs
    | validateInput xs = Just (processData xs)
    | otherwise = Nothing