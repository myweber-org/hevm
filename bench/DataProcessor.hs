
module DataProcessor where

import Data.List.Split (splitOn)

parseCSV :: String -> [[Double]]
parseCSV content = map (map read . splitOn ",") $ lines content

calculateAverages :: [[Double]] -> [Double]
calculateAverages rows
  | null rows = []
  | otherwise = map avg $ transpose rows
  where
    avg xs = sum xs / fromIntegral (length xs)
    transpose = foldr (zipWith (:)) (repeat [])

processCSVData :: String -> [Double]
processCSVData = calculateAverages . parseCSVmodule DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isAlpha)

data ValidationError = InvalidFormat String | MissingField String | InvalidValue String
    deriving (Show, Eq)

type CSVRow = [String]
type Header = [String]

validateCSVRow :: Header -> CSVRow -> Either ValidationError CSVRow
validateCSVRow header row
    | length header /= length row = Left $ InvalidFormat "Column count mismatch"
    | any null row = Left $ MissingField "Empty field detected"
    | not (validateRowData row) = Left $ InvalidValue "Invalid data in row"
    | otherwise = Right row

validateRowData :: CSVRow -> Bool
validateRowData = all validateField
    where
        validateField field
            | all isDigit field = True
            | all isAlpha field = True
            | ' ' `elem` field = True
            | otherwise = False

parseCSV :: String -> Either ValidationError [CSVRow]
parseCSV content = 
    case lines content of
        [] -> Left $ InvalidFormat "Empty CSV content"
        (headerLine:rows) -> 
            let header = splitByComma headerLine
                parsedRows = map (splitByComma) rows
            in case mapM (validateCSVRow header) parsedRows of
                Left err -> Left err
                Right validRows -> Right validRows

splitByComma :: String -> [String]
splitByComma = foldr splitHelper [""]
    where
        splitHelper ',' acc = "":acc
        splitHelper char (x:xs) = (char:x):xs

formatCSV :: [CSVRow] -> String
formatCSV rows = intercalate "\n" $ map (intercalate ",") rows

processCSVData :: String -> Either ValidationError String
processCSVData input = do
    parsed <- parseCSV input
    return $ formatCSV parsed
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processData
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Bool
validateInput xs = not (null xs) && all (>= -1000) xs && all (<= 1000) xs

safeProcess :: [Int] -> Maybe Int
safeProcess xs
    | validateInput xs = Just (sumProcessedData xs)
    | otherwise = Nothingmodule DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage _ [] = []
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | n > length xs = []
    | otherwise = map average $ windows n xs
  where
    windows :: Int -> [a] -> [[a]]
    windows m ys = take (length ys - m + 1) $ zipWith (const . take m) (tails ys) ys
    
    average :: Fractional a => [a] -> a
    average vals = sum vals / fromIntegral (length vals)