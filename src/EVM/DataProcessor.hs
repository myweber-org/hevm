
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

main :: IO ()
main = do
    let sampleData = [-3, 1, 0, 5, -2, 8]
    putStrLn $ "Original data: " ++ show sampleData
    putStrLn $ "Processed data: " ++ show (processData sampleData)
    putStrLn $ "Sum of processed data: " ++ show (sumProcessedData sampleData)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show result
module DataProcessor where

import Data.Time
import Text.CSV

filterCSVByDate :: String -> Day -> Day -> Either String [(String, Day, Double)]
filterCSVByDate csvContent startDate endDate = do
    csv <- parseCSV "input" csvContent
    let rows = tail csv -- Skip header
    processedRows <- mapM parseRow rows
    return $ filter (\(_, date, _) -> date >= startDate && date <= endDate) processedRows
  where
    parseRow :: [String] -> Either String (String, Day, Double)
    parseRow [name, dateStr, valueStr] = do
        date <- parseTimeM True defaultTimeLocale "%Y-%m-%d" dateStr
        value <- maybe (Left "Invalid value") Right (readMaybe valueStr)
        return (name, date, value)
    parseRow _ = Left "Invalid row format"

    readMaybe :: Read a => String -> Maybe a
    readMaybe s = case reads s of
        [(x, "")] -> Just x
        _ -> Nothing
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData