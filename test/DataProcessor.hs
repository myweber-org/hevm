module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbers

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform even (*2)

main :: IO ()
main = do
    let input = [1..10]
    let result = processData input
    print resultmodule DataProcessor where

movingAverage :: (Fractional a) => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
  where
    windows m ys = take (length ys - m + 1) $ zipWith (++) (tails ys) (repeat [])
    average zs = sum zs / fromIntegral (length zs)module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input = case lines input of
    [] -> Left "Empty input"
    rows -> traverse parseRow rows
  where
    parseRow row = case splitOnComma row of
        [] -> Left "Empty row"
        cells -> Right cells

splitOnComma :: String -> [String]
splitOnComma = foldr f [[]]
  where
    f ',' (x:xs) = []:x:xs
    f c (x:xs) = (c:x):xs
    f _ [] = error "Impossible pattern"

validateNumericField :: String -> Either String Int
validateNumericField s
    | all isDigit s = Right (read s)
    | otherwise = Left $ "Invalid numeric field: " ++ s

processCSVData :: CSVData -> Either String [(String, Int)]
processCSVData rows = case rows of
    [] -> Left "No data rows"
    header:dataRows -> do
        validatedRows <- traverse validateRow dataRows
        return validatedRows
  where
    validateRow row = case row of
        [name, valueStr] -> do
            value <- validateNumericField valueStr
            return (name, value)
        _ -> Left $ "Invalid row format: " ++ show row

formatOutput :: [(String, Int)] -> String
formatOutput dataPairs =
    "Name,Value\n" ++
    intercalate "\n" (map (\(name, val) -> name ++ "," ++ show val) dataPairs)

processCSVString :: String -> Either String String
processCSVString input = do
    parsed <- parseCSV input
    processed <- processCSVData parsed
    return $ formatOutput processedmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers