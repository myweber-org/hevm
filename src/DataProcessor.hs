module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let sampleData = [1..10]
    let result = processData sampleData
    putStrLn $ "Original: " ++ show sampleData
    putStrLn $ "Processed: " ++ show result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)
module DataProcessor where

import Data.List (foldl')
import Text.Read (readMaybe)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ',') (lines content)
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitHelper [""]
      where
        splitHelper :: Char -> [String] -> [String]
        splitHelper c (x:xs)
          | c == delimiter = "":x:xs
          | otherwise = (c:x):xs
        splitHelper _ [] = []

calculateColumnAverage :: CSVData -> Int -> Maybe Double
calculateColumnAverage rows columnIndex
  | null validNumbers = Nothing
  | otherwise = Just (sum validNumbers / fromIntegral (length validNumbers))
  where
    extractNumber :: Row -> Maybe Double
    extractNumber row
      | columnIndex < length row = readMaybe (row !! columnIndex)
      | otherwise = Nothing
    
    validNumbers = [x | Just x <- map extractNumber rows]

processCSVFile :: String -> Int -> IO (Maybe Double)
processCSVFile filePath columnIndex = do
  content <- readFile filePath
  let parsedData = parseCSV content
  return $ calculateColumnAverage parsedData columnIndex

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

validateCSV :: CSVData -> Bool
validateCSV [] = True
validateCSV (row:rows) = all (\r -> length r == length row) rows