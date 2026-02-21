module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let input = [1..10]
    let result = processData input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processEvenSquares
module DataProcessor where

import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)

type Row = [Double]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV content = map parseRow (lines content)
  where
    parseRow line = catMaybes $ map parseDouble (splitOn "," line)
    parseDouble s = case reads s of
                     [(val, "")] -> Just val
                     _ -> Nothing

calculateColumnAverages :: CSVData -> [Double]
calculateColumnAverages [] = []
calculateColumnAverages rows = 
    let colCount = length (head rows)
        sums = foldl (zipWith (+)) (replicate colCount 0) rows
        rowCount = fromIntegral (length rows)
    in map (/ rowCount) sums

processCSVFile :: String -> IO [Double]
processCSVFile filename = do
    content <- readFile filename
    let parsed = parseCSV content
    return $ calculateColumnAverages parsed

validateCSVData :: CSVData -> Bool
validateCSVData [] = True
validateCSVData (row:rows) = 
    let expectedLen = length row
    in all (\r -> length r == expectedLen) rowsmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData xs = all (> 0) xs && length xs > 3

combineResults :: [Int] -> [Int] -> [Int]
combineResults xs ys = zipWith (+) (processData xs) (processData ys)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

sumProcessed :: [Int] -> Int
sumProcessed = sum . processEvenSquaresmodule DataProcessor where

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

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedList :: [Int] -> Int
sumProcessedList = sum . processEvenSquares

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list (even numbers squared): " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of processed list: " ++ show (sumProcessedList numbers)module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage windowSize xs
    | windowSize <= 0 = error "Window size must be positive"
    | length xs < windowSize = []
    | otherwise = map average $ windows windowSize xs
  where
    average ws = sum ws / fromIntegral (length ws)
    windows n = takeWhile ((== n) . length) . map (take n) . iterate tailmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs
    | null xs = Nothing
    | any (< -100) xs = Nothing
    | any (> 100) xs = Nothing
    | otherwise = Just xsmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -1000) xs then Just xs else Nothingmodule DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter even
module DataProcessor where

import Data.Time
import Text.CSV

filterCSVByDate :: String -> Day -> Day -> Either String [Record]
filterCSVByDate csvContent startDate endDate = do
    csv <- parseCSV "input" csvContent
    let filtered = filter (isWithinDateRange startDate endDate) csv
    return filtered

isWithinDateRange :: Day -> Day -> Record -> Bool
isWithinDateRange start end record =
    case record of
        (dateStr:_) -> 
            case parseDate dateStr of
                Just date -> date >= start && date <= end
                Nothing   -> False
        _ -> False

parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"module DataProcessor where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.List (foldl')
import Data.Vector (Vector, toList)

type Record = (String, Double, Double, Double)

parseCSV :: BL.ByteString -> Either String (Vector Record)
parseCSV bs = Csv.decode Csv.NoHeader bs

calculateAverages :: Vector Record -> (Double, Double, Double)
calculateAverages records =
    let (sum1, sum2, sum3, count) = foldl' accumulate (0, 0, 0, 0) (toList records)
        accumulate (s1, s2, s3, c) (_, v1, v2, v3) = (s1 + v1, s2 + v2, s3 + v3, c + 1)
    in if count > 0
        then (sum1 / fromIntegral count, sum2 / fromIntegral count, sum3 / fromIntegral count)
        else (0, 0, 0)

processData :: BL.ByteString -> Either String (Double, Double, Double)
processData bs = do
    parsed <- parseCSV bs
    Right $ calculateAverages parsed