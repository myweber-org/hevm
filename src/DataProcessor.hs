
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbers
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = 
    filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

main :: IO ()
main = do
    let numbers = [-3, 1, 4, -1, 5, 9, -2, 6]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

sumProcessed :: [Int] -> Int
sumProcessed = sum . processEvenSquares
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)module DataProcessor where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ",") (lines content)

safeReadDouble :: String -> Maybe Double
safeReadDouble s = case reads s of
    [(val, "")] -> Just val
    _ -> Nothing

calculateColumnAverage :: CSVData -> Int -> Maybe Double
calculateColumnAverage rows colIndex
    | null validValues = Nothing
    | otherwise = Just (sum validValues / fromIntegral (length validValues))
  where
    columnValues = mapMaybe (safeGetColumn colIndex) rows
    validValues = mapMaybe safeReadDouble columnValues
    safeGetColumn idx row
        | idx < length row = Just (row !! idx)
        | otherwise = Nothing

processCSVFile :: String -> IO ()
processCSVFile filename = do
    content <- readFile filename
    let parsed = parseCSV content
    putStrLn "Column averages:"
    mapM_ (printColumnAverage parsed) [0..4]
  where
    printColumnAverage data' idx = do
        putStrLn $ "Column " ++ show idx ++ ": " ++ 
            case calculateColumnAverage data' idx of
                Just avg -> show avg
                Nothing -> "N/A"