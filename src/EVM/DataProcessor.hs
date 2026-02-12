module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just xmodule DataProcessor where

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

calculateTrend :: (Fractional a, Ord a) => [a] -> String
calculateTrend values
    | allIncreasing values = "Increasing trend"
    | allDecreasing values = "Decreasing trend"
    | otherwise = "No clear trend"
    where
        allIncreasing xs = and $ zipWith (<) xs (tail xs)
        allDecreasing xs = and $ zipWith (>) xs (tail xs)module DataProcessor where

import Data.List.Split (splitOn)

parseCSV :: String -> [[Double]]
parseCSV content = map (map read . splitOn ",") $ lines content

calculateAverages :: [[Double]] -> [Double]
calculateAverages rows = 
    if null rows 
    then []
    else map (\col -> sum col / fromIntegral (length col)) $ transpose rows
  where
    transpose :: [[Double]] -> [[Double]]
    transpose [] = []
    transpose ([]:_) = []
    transpose x = map head x : transpose (map tail x)

processCSVData :: String -> [Double]
processCSVData = calculateAverages . parseCSVmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processDatamodule DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isSpace)
import Control.Monad (when)

data ValidationError = InvalidFormat String
                     | MissingField String
                     | InvalidValue String String
                     deriving (Show, Eq)

type CSVRow = [String]
type Header = [String]

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

validateRowLength :: Header -> CSVRow -> Either ValidationError CSVRow
validateRowLength header row
  | length header == length row = Right row
  | otherwise = Left $ InvalidFormat $
      "Expected " ++ show (length header) ++ 
      " fields, got " ++ show (length row)

validateNumericField :: String -> String -> Either ValidationError String
validateNumericField fieldName value
  | all isDigit (trim value) = Right value
  | otherwise = Left $ InvalidValue fieldName $
      "Non-numeric value: " ++ value

validateRequiredField :: String -> String -> Either ValidationError String
validateRequiredField fieldName value
  | not (null (trim value)) = Right value
  | otherwise = Left $ MissingField fieldName

processCSVRow :: Header -> CSVRow -> Either ValidationError [(String, String)]
processCSVRow header rawRow = do
  validatedRow <- validateRowLength header rawRow
  sequence [validateField h v | (h, v) <- zip header validatedRow]
  where
    validateField h v
      | h == "age" = validateNumericField h v
      | h == "name" = validateRequiredField h v
      | otherwise = Right v

formatErrors :: [ValidationError] -> String
formatErrors errors = intercalate "\n" $ map formatError errors
  where
    formatError (InvalidFormat msg) = "Invalid format: " ++ msg
    formatError (MissingField field) = "Missing required field: " ++ field
    formatError (InvalidValue field msg) = 
      "Invalid value in field '" ++ field ++ "': " ++ msg

processCSVData :: Header -> [CSVRow] -> ([(String, String)], String)
processCSVData header rows = (validRows, errorReport)
  where
    results = map (processCSVRow header) rows
    validRows = [row | Right row <- results]
    errors = [err | Left err <- results]
    errorReport = if null errors 
                  then "All rows processed successfully"
                  else "Validation errors:\n" ++ formatErrors errorsmodule DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter evenmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)