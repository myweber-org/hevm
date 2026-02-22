module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input = case lines input of
    [] -> Left "Empty input"
    rows -> traverse parseRow rows
  where
    parseRow :: String -> Either String CSVRow
    parseRow row = case splitByComma row of
        [] -> Left "Empty row"
        cells -> Right cells

    splitByComma :: String -> [String]
    splitByComma = foldr splitter [""]
      where
        splitter ',' (current:rest) = "":current:rest
        splitter char (current:rest) = (char:current):rest

validateNumericColumn :: CSVData -> Int -> Either String CSVData
validateNumericColumn [] _ = Left "No data to validate"
validateNumericColumn rows colIndex
    | colIndex < 0 = Left "Column index cannot be negative"
    | otherwise = traverse validateRow rows
  where
    validateRow :: CSVRow -> Either String CSVRow
    validateRow row
        | colIndex >= length row = Left $ "Row has only " ++ show (length row) ++ " columns"
        | all isDigit (row !! colIndex) = Right row
        | otherwise = Left $ "Non-numeric value in column " ++ show colIndex ++ ": " ++ (row !! colIndex)

formatCSV :: CSVData -> String
formatCSV = intercalate "\n" . map (intercalate ",")

processCSVData :: String -> Int -> Either String String
processCSVData input colIndex = do
    parsed <- parseCSV input
    validated <- validateNumericColumn parsed colIndex
    return $ formatCSV validatedmodule DataProcessor where

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
    putStrLn $ "Original series: " ++ show dataSeries
    putStrLn $ "3-period moving average: " ++ show ma3module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (\x -> x >= -100 && x <= 100) xs 
                   then Just xs 
                   else Nothing

main :: IO ()
main = do
    let sampleData = [1, -2, 3, 0, 5, -10]
    case validateInput sampleData of
        Just validData -> do
            putStrLn $ "Original: " ++ show validData
            putStrLn $ "Processed: " ++ show (processNumbers validData)
            putStrLn $ "Sum: " ++ show (sumProcessed validData)
        Nothing -> putStrLn "Input validation failed: values out of range"module DataProcessor where

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
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

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
    print resultmodule DataProcessor where

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

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

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
validateInput xs = if all (> -100) xs then Just xs else Nothingmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isAlpha)

data ValidationError = InvalidFormat String | InvalidData String
    deriving (Show, Eq)

type CSVRow = [String]
type ValidatedRow = Either ValidationError CSVRow

validateCSVRow :: CSVRow -> ValidatedRow
validateCSVRow row
    | length row /= 3 = Left $ InvalidFormat "Row must have exactly 3 columns"
    | not (all validField row) = Left $ InvalidData "All fields must be non-empty"
    | not (validId (row !! 0)) = Left $ InvalidData "First field must be numeric ID"
    | not (validName (row !! 1)) = Left $ InvalidData "Second field must be alphabetic name"
    | not (validAmount (row !! 2)) = Left $ InvalidData "Third field must be positive numeric amount"
    | otherwise = Right row
  where
    validField = not . null
    validId = all isDigit
    validName = all isAlpha
    validAmount str = case reads str :: [(Double, String)] of
        [(n, "")] -> n > 0
        _ -> False

processCSVData :: [CSVRow] -> [ValidatedRow]
processCSVData = map validateCSVRow

formatResults :: [ValidatedRow] -> String
formatResults rows = intercalate "\n" $ map formatRow rows
  where
    formatRow (Right row) = "VALID: " ++ intercalate "," row
    formatRow (Left err) = "ERROR: " ++ show err

sampleData :: [CSVRow]
sampleData =
    [ ["123", "Alice", "45.50"]
    , ["456", "Bob", "100.00"]
    , ["789", "Charlie", "0.00"]
    , ["abc", "David", "50.00"]
    , ["999", "Eve123", "30.00"]
    , ["111", "Frank", "-10.00"]
    ]

main :: IO ()
main = do
    let results = processCSVData sampleData
    putStrLn $ formatResults results
    let validCount = length $ filter isRight results
    let errorCount = length $ filter isLeft results
    putStrLn $ "\nSummary: " ++ show validCount ++ " valid, " ++ show errorCount ++ " errors"
  where
    isRight (Right _) = True
    isRight _ = False
    isLeft (Left _) = True
    isLeft _ = Falsemodule DataProcessor where

import Data.List (sort, group, maximumBy)
import Data.Ord (comparing)
import qualified Data.Map as Map
import Text.CSV (parseCSV)

type Dataset = [[String]]

parseCSVData :: String -> Either String Dataset
parseCSVData csvString = 
    case parseCSV "input" csvString of
        Left err -> Left $ "CSV parsing error: " ++ err
        Right csv -> Right csv

calculateMean :: [Double] -> Double
calculateMean xs = sum xs / fromIntegral (length xs)

calculateMedian :: [Double] -> Double
calculateMedian xs = 
    let sorted = sort xs
        n = length sorted
        mid = n `div` 2
    in if odd n 
        then sorted !! mid
        else ((sorted !! (mid - 1)) + (sorted !! mid)) / 2.0

calculateMode :: [Double] -> [Double]
calculateMode xs = 
    let freqMap = Map.fromListWith (+) [(x, 1) | x <- xs]
        maxFreq = maximum (Map.elems freqMap)
    in Map.keys (Map.filter (== maxFreq) freqMap)

filterOutliers :: [Double] -> Double -> [Double]
filterOutliers values threshold = 
    let meanVal = calculateMean values
        stdDev = sqrt $ sum (map (\x -> (x - meanVal)^2) values) / fromIntegral (length values)
    in filter (\x -> abs (x - meanVal) <= threshold * stdDev) values

dataSummary :: [Double] -> (Double, Double, [Double], Int)
dataSummary values = 
    let cleanValues = filterOutliers values 2.0
        meanVal = calculateMean cleanValues
        medianVal = calculateMedian cleanValues
        modeVals = calculateMode cleanValues
        count = length cleanValues
    in (meanVal, medianVal, modeVals, count)

processNumericColumn :: Dataset -> Int -> Either String (Double, Double, [Double], Int)
processNumericColumn dataset columnIndex = 
    if null dataset 
    then Left "Empty dataset"
    else case mapM (safeRead . (!! columnIndex)) (tail dataset) of
        Left err -> Left $ "Column parsing error: " ++ err
        Right values -> Right $ dataSummary values
    where
        safeRead :: String -> Either String Double
        safeRead s = case reads s of
            [(x, "")] -> Right x
            _ -> Left $ "Invalid number: " ++ s

findMostFrequentColumn :: Dataset -> Int -> String
findMostFrequentColumn dataset columnIndex = 
    let columnData = map (!! columnIndex) (tail dataset)
        freqMap = Map.fromListWith (+) [(x, 1) | x <- columnData]
    in fst $ maximumBy (comparing snd) (Map.toList freqMap)