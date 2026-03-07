module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Maybe [Int]
validateInput xs
  | null xs = Nothing
  | otherwise = Just xs

main :: IO ()
main = do
  let sampleData = [-3, 1, 0, 5, -2, 8]
  case validateInput sampleData of
    Nothing -> putStrLn "Empty input list"
    Just data' -> do
      let result = processData data'
      putStrLn $ "Original: " ++ show sampleData
      putStrLn $ "Processed: " ++ show resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

import Data.Char (isDigit, isSpace)
import Data.List (intercalate)

type ValidationRule = String -> Bool
type Transformation = String -> String

validateNumeric :: ValidationRule
validateNumeric = all isDigit

validateNonEmpty :: ValidationRule
validateNonEmpty = not . all isSpace

trimWhitespace :: Transformation
trimWhitespace = reverse . dropWhile isSpace . reverse . dropWhile isSpace

normalizeCase :: Transformation
normalizeCase = map toLower
  where toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

processField :: [ValidationRule] -> [Transformation] -> String -> Either String String
processField validations transformations input =
  case validateAll validations input of
    Left err -> Left err
    Right validated -> Right $ applyTransformations transformations validated

validateAll :: [ValidationRule] -> String -> Either String String
validateAll rules input = foldl validateStep (Right input) rules
  where validateStep (Right val) rule = if rule val then Right val else Left "Validation failed"
        validateStep left _ = left

applyTransformations :: [Transformation] -> String -> String
applyTransformations = foldl (flip (.)) id

processCSVRow :: [String] -> Either String [String]
processCSVRow row = sequence $ zipWith processField validationRules transformationRules row
  where validationRules = [validateNonEmpty, validateNumeric, validateNonEmpty]
        transformationRules = [trimWhitespace, id, normalizeCase]

formatOutput :: [String] -> String
formatOutput fields = intercalate "|" fields

main :: IO ()
main = do
  let testData = [" 123 ", "456", "  TEST  "]
  case processCSVRow testData of
    Left err -> putStrLn $ "Error: " ++ err
    Right processed -> putStrLn $ "Processed: " ++ formatOutput processedmodule DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter evenmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbers

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of positive doubles: " ++ show (sumPositiveDoubles numbers)module DataProcessor where

import Data.Char (toUpper)

processData :: [(String, Int)] -> [(String, Int)]
processData = map (\(name, val) -> (map toUpper name, val * 2)) . filter (\(_, val) -> val > 0)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

import Data.List (tails)

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map avg $ filter (\window -> length window == n) $ tails xs
  where
    avg window = sum window / fromIntegral n

smoothData :: Int -> [Double] -> [Double]
smoothData windowSize dataPoints = movingAverage windowSize dataPoints

calculateTrend :: [Double] -> Double
calculateTrend values
    | null values = 0.0
    | otherwise = (last values - head values) / fromIntegral (length values - 1)

processDataset :: Int -> [Double] -> (Double, [Double])
processDataset windowSize dataset =
    let smoothed = smoothData windowSize dataset
        trend = calculateTrend smoothed
    in (trend, smoothed)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

processOddCubes :: [Int] -> [Int]
processOddCubes = filterAndTransform odd (\x -> x * x * x)

sumProcessedData :: (Int -> Bool) -> (Int -> Int) -> [Int] -> Int
sumProcessedData predicate transformer = sum . filterAndTransform predicate transformermodule DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input = mapM parseRow (lines input)
  where
    parseRow line = case splitOnComma line of
        [] -> Left "Empty row"
        cells -> Right (map trim cells)

    splitOnComma :: String -> [String]
    splitOnComma [] = []
    splitOnComma str = 
        let (cell, rest) = break (== ',') str
        in cell : case rest of
                    ',' : xs -> splitOnComma xs
                    _        -> []

    trim :: String -> String
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

validateNumericColumn :: CSVData -> Int -> Either String CSVData
validateNumericColumn [] _ = Right []
validateNumericColumn (row:rows) colIndex
    | colIndex < 0 = Left "Column index cannot be negative"
    | colIndex >= length row = Left "Column index out of bounds"
    | all isNumeric (row !! colIndex) = do
        rest <- validateNumericColumn rows colIndex
        Right (row : rest)
    | otherwise = Left $ "Non-numeric value in column " ++ show colIndex
  where
    isNumeric = all isDigit

calculateColumnAverage :: CSVData -> Int -> Either String Double
calculateColumnAverage rows colIndex = do
    validated <- validateNumericColumn rows colIndex
    let values = map (read . (!! colIndex)) validated
    if null values 
        then Left "No data to calculate average"
        else Right (sum values / fromIntegral (length values))

formatCSVOutput :: CSVData -> String
formatCSVOutput = intercalate "\n" . map (intercalate ",")

processCSVFile :: String -> Int -> Either String (String, Double)
processCSVFile content colIndex = do
    parsed <- parseCSV content
    average <- calculateColumnAverage parsed colIndex
    Right (formatCSVOutput parsed, average)
module DataProcessor where

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