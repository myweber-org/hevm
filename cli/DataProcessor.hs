
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (>0) (*2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Bool
validateInput xs = not (null xs) && all (> -100) xs

safeProcess :: [Int] -> Maybe Int
safeProcess xs
    | validateInput xs = Just (sumProcessed xs)
    | otherwise = Nothingmodule DataProcessor where

import Data.List (transpose)
import Text.Read (readMaybe)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> Either String CSVData
parseCSV content = case lines content of
    [] -> Left "Empty file"
    rows -> let parsedRows = map (splitOn ',') rows
            in if allRowsSameLength parsedRows
               then Right parsedRows
               else Left "Rows have inconsistent column counts"
    where
        splitOn :: Char -> String -> Row
        splitOn delimiter = foldr step [[]]
            where
                step ch (x:xs) | ch == delimiter = []:x:xs
                step ch (x:xs) = (ch:x):xs

allRowsSameLength :: [Row] -> Bool
allRowsSameLength [] = True
allRowsSameLength (x:xs) = all ((== length x) . length) xs

validateNumericColumns :: CSVData -> [Int] -> Either String [[Double]]
validateNumericColumns rows indices = 
    mapM validateRow rows >>= validateColumnCount
    where
        validateRow row = mapM (validateCell . (row !!)) indices
        validateCell str = case readMaybe str of
            Just num -> Right num
            Nothing -> Left $ "Invalid numeric value: " ++ str
        validateColumnCount nums = 
            if all ((== length (head nums)) . length) nums
            then Right nums
            else Left "Numeric data validation produced inconsistent rows"

calculateColumnAverages :: [[Double]] -> [Double]
calculateColumnAverages = map average . transpose
    where
        average xs = sum xs / fromIntegral (length xs)

processCSVFile :: String -> [Int] -> Either String [Double]
processCSVFile content numericColumns = do
    csvData <- parseCSV content
    numericData <- validateNumericColumns csvData numericColumns
    return $ calculateColumnAverages numericData
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

main :: IO ()
main = do
    let numbers = [-3, 1, 4, -1, 5, 9, -2, 6]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)