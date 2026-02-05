
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

calculateColumnAverage :: CSVData -> Int -> Maybe Double
calculateColumnAverage rows columnIndex
  | null validValues = Nothing
  | otherwise = Just (sum validValues / fromIntegral (length validValues))
  where
    extractValue :: Row -> Maybe Double
    extractValue row
      | columnIndex < length row = safeReadDouble (row !! columnIndex)
      | otherwise = Nothing
    
    validValues = [val | Just val <- map extractValue rows]

processCSVFile :: String -> Int -> IO (Maybe Double)
processCSVFile filePath columnIndex = do
  content <- readFile filePath
  let parsedData = parseCSV content
  return $ calculateColumnAverage parsedData columnIndex

validateCSV :: CSVData -> Bool
validateCSV [] = True
validateCSV (row:rows) = all (== length row) (map length rows)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers