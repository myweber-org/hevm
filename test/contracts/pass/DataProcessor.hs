
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessed :: (Int -> Int) -> [Int] -> Int
sumProcessed f = sum . map f

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Even squares: " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of even squares: " ++ show (sumProcessed (\x -> x * x) (filter even numbers))
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedData :: (Int -> Bool) -> (Int -> Int) -> [Int] -> Int
sumProcessedData predicate transformer = sum . filterAndTransform predicate transformer

main :: IO ()
main = do
    let dataSet = [1..10]
    putStrLn $ "Original data: " ++ show dataSet
    putStrLn $ "Even squares: " ++ show (processEvenSquares dataSet)
    putStrLn $ "Sum of even squares: " ++ show (sumProcessedData even (\x -> x * x) dataSet)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData = all (> 0) . processData

main :: IO ()
main = do
    let sampleData = [-3, 1, 0, 5, -2, 8]
    putStrLn $ "Original data: " ++ show sampleData
    putStrLn $ "Processed data: " ++ show (processData sampleData)
    putStrLn $ "Validation result: " ++ show (validateData sampleData)
module DataProcessor where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Record = (String, Double, Double)

parseCSVLine :: String -> Maybe Record
parseCSVLine line = case splitOn "," line of
    [name, val1, val2] -> 
        case (reads val1, reads val2) of
            ([(v1, "")], [(v2, "")]) -> Just (name, v1, v2)
            _ -> Nothing
    _ -> Nothing

calculateAverages :: [Record] -> (Double, Double)
calculateAverages records = 
    let (sum1, sum2) = foldr (\(_, v1, v2) (s1, s2) -> (s1 + v1, s2 + v2)) (0, 0) records
        count = fromIntegral (length records)
    in (sum1 / count, sum2 / count)

processCSVData :: String -> Maybe (Double, Double)
processCSVData csvContent = 
    let records = mapMaybe parseCSVLine (lines csvContent)
    in if null records 
        then Nothing 
        else Just (calculateAverages records)

validateRecord :: Record -> Bool
validateRecord (_, v1, v2) = v1 >= 0 && v2 >= 0

filterValidRecords :: [Record] -> [Record]
filterValidRecords = filter validateRecordmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    putStrLn $ "Processed data: " ++ show result