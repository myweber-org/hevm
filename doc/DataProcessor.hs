
module DataProcessor where

import Data.List (foldl')
import Text.Read (readMaybe)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ',') (lines content)
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitter [[]]
      where
        splitter c (x:xs)
          | c == delimiter = []:x:xs
          | otherwise = (c:x):xs

safeReadDouble :: String -> Maybe Double
safeReadDouble = readMaybe

computeColumnAverage :: CSVData -> Int -> Maybe Double
computeColumnAverage [] _ = Nothing
computeColumnAverage rows colIndex
  | colIndex < 0 = Nothing
  | otherwise = case validValues of
      [] -> Nothing
      vs -> Just (sum vs / fromIntegral (length vs))
  where
    extractValue :: Row -> Maybe Double
    extractValue row
      | colIndex < length row = safeReadDouble (row !! colIndex)
      | otherwise = Nothing
    
    validValues = [v | Just v <- map extractValue rows]

processCSVFile :: String -> Int -> IO (Maybe Double)
processCSVFile filePath column = do
  content <- readFile filePath
  let parsedData = parseCSV content
  return $ computeColumnAverage parsedData column
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

safeHeadProcessed :: [Int] -> Maybe Int
safeHeadProcessed [] = Nothing
safeHeadProcessed xs = Just (head (processNumbers xs))