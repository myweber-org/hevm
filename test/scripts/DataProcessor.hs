module DataProcessor where

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

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processDatamodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData xs = all (> 0) xs && length xs > 3

combineResults :: [Int] -> [Int] -> [Int]
combineResults xs ys = zipWith (+) (processData xs) (processData ys)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbers

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

validateInput :: [Int] -> Either String [Int]
validateInput [] = Left "Empty input list"
validateInput xs
    | any (< 0) xs = Left "Negative numbers not allowed"
    | otherwise = Right xsmodule DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

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
splitOnComma [] = []
splitOnComma str = 
    let (cell, rest) = break (== ',') str
    in cell : case rest of
                ',' : xs -> splitOnComma xs
                _ -> []

validateNumericField :: String -> Either String Int
validateNumericField str
    | all isDigit str = Right (read str)
    | otherwise = Left $ "Invalid numeric value: " ++ str

processCSVData :: CSVData -> Either String [(String, Int)]
processCSVData rows = case rows of
    [] -> Left "No data to process"
    header : dataRows -> do
        let expectedHeader = ["Name", "Age", "Score"]
        if header == expectedHeader
            then mapM processRow dataRows
            else Left $ "Invalid header. Expected: " ++ intercalate "," expectedHeader
  where
    processRow [name, ageStr, scoreStr] = do
        age <- validateNumericField ageStr
        score <- validateNumericField scoreStr
        if age >= 0 && age <= 150 && score >= 0 && score <= 100
            then Right (name, score)
            else Left $ "Invalid data in row: " ++ name
    processRow _ = Left "Row has incorrect number of columns"

formatOutput :: [(String, Int)] -> String
formatOutput results =
    "Processed Results:\n" ++
    intercalate "\n" (map (\(name, score) -> name ++ ": " ++ show score) results)

main :: IO ()
main = do
    let csvContent = "Name,Age,Score\nAlice,25,85\nBob,30,92\nCharlie,17,78"
    case parseCSV csvContent >>= processCSVData of
        Left err -> putStrLn $ "Error: " ++ err
        Right results -> putStrLn $ formatOutput resultsmodule DataProcessor where

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

main :: IO ()
main = do
    let sampleData = [1..10]
    putStrLn $ "Original data: " ++ show sampleData
    putStrLn $ "Processed data: " ++ show (processEvenSquares sampleData)
    putStrLn $ "Sum of processed data: " ++ show (sumProcessedData sampleData)module DataProcessor where

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
  where
    windows :: Int -> [a] -> [[a]]
    windows m = takeWhile ((== m) . length) . map (take m) . iterate tail
    
    average :: [Double] -> Double
    average ys = sum ys / fromIntegral (length ys)

safeMovingAverage :: Int -> [Double] -> Maybe [Double]
safeMovingAverage n xs
    | n <= 0 = Nothing
    | otherwise = Just $ movingAverage n xs

testData :: [Double]
testData = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]module DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter evenmodule DataProcessor where

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

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Even squares: " ++ show (processEvenSquares numbers)
    putStrLn $ "First element: " ++ show (safeHead numbers)