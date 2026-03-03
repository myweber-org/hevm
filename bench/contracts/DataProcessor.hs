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
        splitter char acc@(x:xs)
          | char == delimiter = []:acc
          | otherwise = (char:x):xs

safeReadDouble :: String -> Maybe Double
safeReadDouble = readMaybe

calculateColumnAverage :: CSVData -> Int -> Maybe Double
calculateColumnAverage [] _ = Nothing
calculateColumnAverage rows columnIndex
  | columnIndex < 0 = Nothing
  | otherwise = case validValues of
      [] -> Nothing
      vs -> Just (sum vs / fromIntegral (length vs))
  where
    extractValue :: Row -> Maybe Double
    extractValue row
      | columnIndex < length row = safeReadDouble (row !! columnIndex)
      | otherwise = Nothing
    
    validValues = [v | Just v <- map extractValue rows]

processCSVFile :: String -> Int -> IO (Maybe Double)
processCSVFile filePath column = do
  content <- readFile filePath
  let parsed = parseCSV content
  return $ calculateColumnAverage parsed column