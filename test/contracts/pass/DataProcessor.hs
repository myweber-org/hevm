
module DataProcessor where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Record = (String, Double)

parseCSVLine :: String -> Maybe Record
parseCSVLine line = case splitOn "," line of
    [name, valueStr] -> case reads valueStr of
        [(value, "")] -> Just (name, value)
        _ -> Nothing
    _ -> Nothing

parseCSV :: String -> [Record]
parseCSV content = mapMaybe parseCSVLine (lines content)

calculateAverage :: [Record] -> Double
calculateAverage records = 
    if null records 
    then 0.0 
    else total / fromIntegral (length records)
  where
    total = sum (map snd records)

processCSVData :: String -> (Double, Int)
processCSVData csvContent = 
    let records = parseCSV csvContent
        avg = calculateAverage records
        count = length records
    in (avg, count)

main :: IO ()
main = do
    let sampleData = "Alice,85.5\nBob,92.0\nCharlie,78.5\nDiana,88.0"
    let (average, recordCount) = processCSVData sampleData
    putStrLn $ "Processed " ++ show recordCount ++ " records"
    putStrLn $ "Average value: " ++ show averagemodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = all (\x -> x >= -100 && x <= 100) xs

safeProcess :: [Int] -> Maybe [Int]
safeProcess xs
    | validateInput xs = Just (processData xs)
    | otherwise = Nothingmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processData

validateInput :: [Int] -> Maybe [Int]
validateInput xs = 
    if all (\x -> x >= -100 && x <= 100) xs
        then Just xs
        else Nothing

processValidatedData :: [Int] -> Maybe Int
processValidatedData xs = 
    sumPositiveDoubles <$> validateInput xs