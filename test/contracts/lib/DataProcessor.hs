
module DataProcessor where

import Data.Time
import Text.CSV

filterCSVByDate :: Day -> Day -> CSV -> CSV
filterCSVByDate startDate endDate (header:rows) =
    header : filter (isWithinDateRange startDate endDate) rows
  where
    isWithinDateRange start end row =
        case parseDate (head row) of
            Just date -> date >= start && date <= end
            Nothing   -> False

    parseDate :: String -> Maybe Day
    parseDate str = parseTimeM True defaultTimeLocale "%Y-%m-%d" str

processData :: CSV -> (Double, Double)
processData (header:rows) =
    let values = map (read . (!!1)) rows
        avg = sum values / fromIntegral (length values)
        variance = sum (map (\x -> (x - avg) ** 2) values) / fromIntegral (length values)
    in (avg, variance)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 10) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> 0) xs then Just xs else Nothing

main :: IO ()
main = do
    let sampleData = [5, 12, 8, 20, 3, 15]
    print $ processData sampleData
    print $ sumProcessedData sampleData
    print $ validateInput sampleData
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 10) (* 2)

validateInput :: [Int] -> Maybe [Int]
validateInput [] = Nothing
validateInput xs = Just xs

main :: IO ()
main = do
    let sampleData = [5, 12, 8, 20, 3, 15]
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

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x