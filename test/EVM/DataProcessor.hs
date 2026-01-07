module DataProcessor where

import Data.List (foldl')
import Text.Read (readMaybe)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV = map (splitOn ',') . lines
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitHelper [[]]
      where
        splitHelper :: Char -> [String] -> [String]
        splitHelper c (x:xs)
          | c == delimiter = [] : x : xs
          | otherwise = (c:x) : xs

calculateColumnAverage :: CSVData -> Int -> Maybe Double
calculateColumnAverage [] _ = Nothing
calculateColumnAverage rows columnIndex
  | columnIndex < 0 = Nothing
  | otherwise = case validNumbers of
      [] -> Nothing
      nums -> Just (sum nums / fromIntegral (length nums))
  where
    extractNumbers :: CSVData -> [Double]
    extractNumbers = mapMaybe (getColumn columnIndex)
    
    getColumn :: Int -> Row -> Maybe Double
    getColumn idx row
      | idx < length row = readMaybe (row !! idx)
      | otherwise = Nothing
    
    mapMaybe :: (a -> Maybe b) -> [a] -> [b]
    mapMaybe f = foldr (\x acc -> case f x of
      Just val -> val : acc
      Nothing -> acc) []
    
    validNumbers = extractNumbers rows

processCSVFile :: FilePath -> Int -> IO (Maybe Double)
processCSVFile filePath columnIndex = do
  content <- readFile filePath
  let parsedData = parseCSV content
  return $ calculateColumnAverage parsedData columnIndex

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

validateCSV :: CSVData -> Bool
validateCSV [] = True
validateCSV (firstRow:rows) = all (\row -> length row == length firstRow) rows