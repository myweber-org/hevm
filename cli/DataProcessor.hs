module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = not (null xs) && all (\x -> x >= -100 && x <= 100) xs

main :: IO ()
main = do
    let sampleData = [1, -2, 3, 0, 5, -8]
    if validateInput sampleData
        then print $ processData sampleData
        else putStrLn "Invalid input data"module DataProcessor where

import Data.Char (isDigit, isAlpha)

-- Validate that a string contains only digits
validateNumeric :: String -> Bool
validateNumeric = all isDigit

-- Validate that a string contains only alphabetic characters
validateAlpha :: String -> Bool
validateAlpha = all isAlpha

-- Transform a string to uppercase
toUpperString :: String -> String
toUpperString = map toUpper
  where
    toUpper c
        | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
        | otherwise = c

-- Process a list of strings: validate and transform
processData :: [String] -> [String]
processData = map toUpperString . filter validateAlpha

-- Calculate statistics on numeric strings
calculateStats :: [String] -> (Int, Double)
calculateStats strs = (count, avg)
  where
    nums = map read (filter validateNumeric strs) :: [Double]
    count = length nums
    avg = if count > 0 then sum nums / fromIntegral count else 0.0

-- Safe division with error handling
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

-- Main processing pipeline
processPipeline :: [String] -> IO ()
processPipeline input = do
    putStrLn "Processing data..."
    let cleaned = processData input
    putStrLn $ "Cleaned data: " ++ show cleaned
    
    let stats = calculateStats input
    putStrLn $ "Numeric stats - Count: " ++ show (fst stats) ++ ", Average: " ++ show (snd stats)
    
    case safeDivide (snd stats) (fromIntegral $ fst stats) of
        Just val -> putStrLn $ "Average per valid numeric: " ++ show val
        Nothing -> putStrLn "No valid numeric data for division"module DataProcessor where

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

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
    let dataSeries = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
    putStrLn $ "Original data: " ++ show dataSeries
    putStrLn $ "3-period moving average: " ++ show (movingAverage 3 dataSeries)
    putStrLn $ "5-period moving average: " ++ show (movingAverage 5 dataSeries)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input = 
    if null input 
    then Left "Empty input"
    else Right $ map parseRow (lines input)
  where
    parseRow :: String -> CSVRow
    parseRow = splitByComma
    
    splitByComma :: String -> [String]
    splitByComma [] = []
    splitByComma str = 
        let (field, rest) = break (== ',') str
        in trim field : case rest of
            ',' : xs -> splitByComma xs
            _        -> []
    
    trim :: String -> String
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

validateNumericField :: String -> Either String Int
validateNumericField str
    | all isDigit trimmed = Right (read trimmed)
    | otherwise = Left $ "Invalid numeric value: " ++ str
  where
    trimmed = trim str

processCSVData :: CSVData -> Either String [(String, Int)]
processCSVData [] = Left "No data to process"
processCSVData rows = 
    sequence $ map validateRow rows
  where
    validateRow :: CSVRow -> Either String (String, Int)
    validateRow [name, valueStr] = do
        numericValue <- validateNumericField valueStr
        return (trim name, numericValue)
    validateRow row = 
        Left $ "Invalid row format: " ++ intercalate "," row

calculateTotal :: [(String, Int)] -> Int
calculateTotal = sum . map snd

main :: IO ()
main = do
    let sampleData = "Alice, 100\nBob, 200\nCharlie, invalid"
    case parseCSV sampleData of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right csv -> 
            case processCSVData csv of
                Left err -> putStrLn $ "Validation error: " ++ err
                Right processed -> do
                    putStrLn "Processed data:"
                    mapM_ (\(name, val) -> putStrLn $ name ++ ": " ++ show val) processed
                    putStrLn $ "Total: " ++ show (calculateTotal processed)