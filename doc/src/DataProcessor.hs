module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

main :: IO ()
main = do
    let numbers = [1, -2, 3, -4, 5]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed numbers: " ++ show (sumProcessed numbers)
module DataProcessor where

import Data.List.Split (splitOn)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ",") (lines content)

numericColumns :: CSVData -> [[Double]]
numericColumns [] = []
numericColumns (header:rows) = 
    transpose $ map (map read) $ filter (all isNumeric) $ transpose rows
  where
    isNumeric str = case reads str :: [(Double, String)] of
        [(_, "")] -> True
        _         -> False
    transpose = foldr (zipWith (:)) (repeat [])

calculateAverages :: [[Double]] -> [Double]
calculateAverages columns = map average columns
  where
    average xs = sum xs / fromIntegral (length xs)

processCSVFile :: String -> IO [Double]
processCSVFile filename = do
    content <- readFile filename
    let parsed = parseCSV content
    let columns = numericColumns parsed
    return $ calculateAverages columns

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

validateCSV :: CSVData -> Maybe String
validateCSV [] = Just "Empty CSV data"
validateCSV [_] = Just "Only header row present"
validateCSV (header:rows)
    | length header == 0 = Just "Empty header"
    | any null header = Just "Empty column names in header"
    | any (\row -> length row /= length header) rows = Just "Inconsistent row lengths"
    | otherwise = Nothingmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -100) xs then Just xs else Nothing