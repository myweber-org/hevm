
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbersmodule DataProcessor where

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
module DataProcessor where

import Data.List (foldl')
import Text.Read (readMaybe)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV = map (splitOn ',') . lines
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitter [[]]
      where
        splitter c (x:xs)
          | c == delimiter = []:x:xs
          | otherwise = (c:x):xs

calculateColumnAverages :: CSVData -> Maybe [Double]
calculateColumnAverages [] = Nothing
calculateColumnAverages rows@(header:_) = 
  let numericRows = map (map parseDouble) (tail rows)
      columnCount = length header
      sums = foldl' (zipWith (+)) (replicate columnCount 0.0) numericRows
      counts = foldl' (zipWith (+)) (replicate columnCount 0) 
               (map (map (\x -> if isNaN x then 0 else 1)) numericRows)
  in Just $ zipWith (/) sums (map fromIntegral counts)
  where
    parseDouble :: String -> Double
    parseDouble s = case readMaybe s of
      Just n -> n
      Nothing -> 0/0  -- NaN for invalid numbers

processCSVFile :: FilePath -> IO (Maybe [Double])
processCSVFile path = do
  content <- readFile path
  let parsed = parseCSV content
  return $ calculateColumnAverages parsed

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print result