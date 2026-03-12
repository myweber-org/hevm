module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input = 
    if null input
    then Left "Empty input"
    else Right $ map (splitOn ',') (lines input)
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitHelper [""]
      where
        splitHelper ch (x:xs)
            | ch == delimiter = "":x:xs
            | otherwise = (ch:x):xs

validateNumericColumn :: CSVData -> Int -> Either String CSVData
validateNumericColumn [] _ = Left "Empty data"
validateNumericColumn rows colIndex
    | colIndex < 0 = Left "Negative column index"
    | any (\row -> length row <= colIndex) rows = 
        Left $ "Column index " ++ show colIndex ++ " out of bounds"
    | not (all (all isDigit . dropWhile (== '-')) (map (!! colIndex) rows)) =
        Left $ "Column " ++ show colIndex ++ " contains non-numeric values"
    | otherwise = Right rows

calculateColumnSum :: CSVData -> Int -> Either String Double
calculateColumnSum rows colIndex = do
    validated <- validateNumericColumn rows colIndex
    let values = map (read . (!! colIndex)) validated
    return $ sum values

formatCSVOutput :: CSVData -> String
formatCSVOutput = intercalate "\n" . map (intercalate ",")

processCSVData :: String -> Int -> Either String (String, Double)
processCSVData rawData colIndex = do
    parsed <- parseCSV rawData
    sumResult <- calculateColumnSum parsed colIndex
    return (formatCSVOutput parsed, sumResult)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (* 2)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [1, -2, 3, -4, 5]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -100) xs then Just xs else Nothingmodule DataProcessor where

import Data.List (foldl')
import Data.Maybe (catMaybes)

data StatRecord = StatRecord
    { value :: Double
    , category :: String
    } deriving (Show, Eq)

parseCSVLine :: String -> Maybe StatRecord
parseCSVLine line = case words line of
    [valStr, cat] -> case reads valStr of
        [(val, "")] -> Just $ StatRecord val cat
        _ -> Nothing
    _ -> Nothing

parseCSVData :: String -> [StatRecord]
parseCSVData = catMaybes . map parseCSVLine . lines

computeStats :: [StatRecord] -> (Double, Double, Double)
computeStats records =
    let values = map value records
        count = fromIntegral $ length values
        sumVals = foldl' (+) 0 values
        mean = sumVals / count
        variance = foldl' (\acc x -> acc + (x - mean) ** 2) 0 values / count
        stdDev = sqrt variance
    in (mean, variance, stdDev)

filterByCategory :: String -> [StatRecord] -> [StatRecord]
filterByCategory cat = filter (\r -> category r == cat)

processCSVData :: String -> String -> IO ()
processCSVData cat input = do
    let records = parseCSVData input
        filtered = filterByCategory cat records
        (mean, var, std) = computeStats filtered
    
    putStrLn $ "Category: " ++ cat
    putStrLn $ "Record count: " ++ show (length filtered)
    putStrLn $ "Mean: " ++ show mean
    putStrLn $ "Variance: " ++ show var
    putStrLn $ "Standard deviation: " ++ show std