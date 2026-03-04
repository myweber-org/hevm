module DataProcessor where

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

processData :: [Int] -> [Int]
processData = filterAndTransform even (*2)

main :: IO ()
main = do
    let input = [1..10]
    let result = processData input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show resultmodule DataProcessor where

import Data.List (tails)

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | n > length xs = error "Window size exceeds list length"
    | otherwise = map average $ filter (\window -> length window == n) $ tails xs
  where
    average :: [Double] -> Double
    average ws = sum ws / fromIntegral n

safeMovingAverage :: Int -> [Double] -> Maybe [Double]
safeMovingAverage n xs
    | n <= 0 = Nothing
    | n > length xs = Nothing
    | otherwise = Just $ movingAverage n xs

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
    let dataSeries = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
    putStrLn "Original data:"
    print dataSeries
    putStrLn "\nMoving average with window 3:"
    print $ movingAverage 3 dataSeries
    putStrLn "\nSafe moving average with invalid window 15:"
    print $ safeMovingAverage 15 dataSeriesmodule DataProcessor where

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

smoothData :: [Double] -> [Double]
smoothData = movingAverage 3

testData :: [Double]
testData = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input
    | null input = Left "Empty input"
    | otherwise = Right $ map parseRow (lines input)
  where
    parseRow line = splitOnComma line

splitOnComma :: String -> CSVRow
splitOnComma [] = []
splitOnComma line = 
    let (cell, rest) = break (== ',') line
    in trim cell : case rest of
        ',' : xs -> splitOnComma xs
        _ -> []

trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

validateNumericField :: String -> Either String Int
validateNumericField str
    | all isDigit trimmed = Right (read trimmed)
    | otherwise = Left $ "Invalid numeric value: " ++ str
  where
    trimmed = trim str

processCSVData :: CSVData -> Either String [(String, Int)]
processCSVData rows = case rows of
    [] -> Left "No data rows"
    header:dataRows -> 
        if length header >= 2
        then mapM processRow dataRows
        else Left "Invalid header format"
  where
    processRow row = case row of
        [name, valueStr] -> do
            value <- validateNumericField valueStr
            return (trim name, value)
        _ -> Left $ "Invalid row format: " ++ show row

formatOutput :: [(String, Int)] -> String
formatOutput dataPairs = 
    "Processed Data:\n" ++ 
    intercalate "\n" (map (\(name, val) -> name ++ ": " ++ show val) dataPairs)

main :: IO ()
main = do
    let csvContent = "Name,Value\nAlice,25\nBob,30\nCharlie,invalid"
    case parseCSV csvContent >>= processCSVData of
        Left err -> putStrLn $ "Error: " ++ err
        Right result -> putStrLn $ formatOutput resultmodule DataProcessor where

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
sumProcessedData = sum . processData

main :: IO ()
main = do
    let inputData = [-3, -1, 0, 2, 5, 8]
    let result = sumProcessedData inputData
    putStrLn $ "Sum of processed data: " ++ show result