
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenNumbers :: [Int] -> [Int]
processEvenNumbers = filterAndTransform even (*2)

processOddNumbers :: [Int] -> [Int]
processOddNumbers = filterAndTransform odd (+1)

sumProcessedData :: (Int -> Bool) -> (Int -> Int) -> [Int] -> Int
sumProcessedData predicate transformer = 
    sum . filterAndTransform predicate transformer

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original numbers: " ++ show numbers
    putStrLn $ "Even numbers doubled: " ++ show (processEvenNumbers numbers)
    putStrLn $ "Odd numbers incremented: " ++ show (processOddNumbers numbers)
    putStrLn $ "Sum of processed even numbers: " ++ show (sumProcessedData even (*2) numbers)
module DataProcessor where

import Data.Time
import Data.Time.Format
import Data.List
import Data.Char

type CSVRow = [String]

parseCSV :: String -> [CSVRow]
parseCSV = map (splitOn ',') . lines
  where
    splitOn :: Char -> String -> [String]
    splitOn delim = foldr f [[]]
      where
        f c (x:xs) | c == delim = []:x:xs
                   | otherwise = (c:x):xs

filterByDateRange :: Day -> Day -> [CSVRow] -> [CSVRow]
filterByDateRange start end rows = filter inRange rows
  where
    inRange row = case parseDate (head row) of
      Just date -> date >= start && date <= end
      Nothing -> False
    parseDate :: String -> Maybe Day
    parseDate str = parseTimeM True defaultTimeLocale "%Y-%m-%d" str

sumNumericColumn :: Int -> [CSVRow] -> Double
sumNumericColumn colIdx rows = sum $ map (readColumn colIdx) rows
  where
    readColumn idx row = 
      if idx < length row 
        then maybe 0 id (safeRead (row !! idx))
        else 0
    safeRead s = case reads s of
      [(x, "")] -> Just x
      _ -> Nothing

processCSVData :: String -> Day -> Day -> Int -> (Int, Double)
processCSVData csvStr startDate endDate colIdx =
  let rows = parseCSV csvStr
      filtered = filterByDateRange startDate endDate rows
      total = sumNumericColumn colIdx filtered
  in (length filtered, total)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, 0, 5, -8]
    let result = processNumbers input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result