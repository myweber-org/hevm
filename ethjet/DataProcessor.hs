module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = all (\x -> x >= -100 && x <= 100) xs

main :: IO ()
main = do
    let sampleData = [-5, 2, 0, 8, -3, 10]
    if validateInput sampleData
        then do
            putStrLn "Processing valid data..."
            let result = processData sampleData
            putStrLn $ "Input: " ++ show sampleData
            putStrLn $ "Result: " ++ show result
        else
            putStrLn "Invalid input detected"module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage windowSize xs
    | windowSize <= 0 = error "Window size must be positive"
    | length xs < windowSize = []
    | otherwise = map average $ windows windowSize xs
  where
    average ws = sum ws / fromIntegral (length ws)
    windows n = takeWhile (\w -> length w == n) . map (take n) . tailsmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Bool
validateInput = all (\x -> x >= -100 && x <= 100)

safeProcessData :: [Int] -> Maybe [Int]
safeProcessData xs
    | validateInput xs = Just (processData xs)
    | otherwise = Nothing
module DataProcessor where

import Data.Time
import Data.Time.Format
import Data.List
import Data.Char

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> CSVData
parseCSV = map (splitOn ',') . lines
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr f [[]]
      where
        f c (x:xs) | c == delimiter = []:x:xs
                   | otherwise = (c:x):xs

filterByDateRange :: CSVData -> String -> String -> CSVData
filterByDateRange [] _ _ = []
filterByDateRange (header:rows) startDate endDate = 
    header : filter (isInRange startDate endDate) rows
  where
    isInRange start end row = 
        case parseDate (head row) of
            Just date -> date >= parseDateString start && date <= parseDateString end
            Nothing -> False
    
    parseDate :: String -> Maybe Day
    parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"
    
    parseDateString :: String -> Day
    parseDateString str = 
        case parseDate str of
            Just d -> d
            Nothing -> fromGregorian 1900 1 1

calculateMonthlyTotals :: CSVData -> [(String, Double)]
calculateMonthlyTotals [] = []
calculateMonthlyTotals (header:rows) = 
    map (\(month, amounts) -> (month, sum amounts)) grouped
  where
    extractMonth row = take 7 (head row)
    extractAmount row = read (row !! 1) :: Double
    
    grouped = groupBy (\a b -> fst a == fst b) 
             $ sortOn fst 
             $ map (\row -> (extractMonth row, extractAmount row)) rows

formatOutput :: [(String, Double)] -> String
formatOutput totals = 
    "Monthly Report:\n" ++ 
    unlines (map (\(month, total) -> month ++ ": " ++ show total) totals)