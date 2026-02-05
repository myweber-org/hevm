
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData = all (> 0) . processData

main :: IO ()
main = do
    let sampleData = [-3, 2, 0, 5, -1, 8]
    putStrLn $ "Original data: " ++ show sampleData
    putStrLn $ "Processed data: " ++ show (processData sampleData)
    putStrLn $ "Data validation: " ++ show (validateData sampleData)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -100) xs then Just xs else Nothing
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

computeColumnAverage :: CSVData -> Int -> Maybe Double
computeColumnAverage rows columnIndex
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
  return $ computeColumnAverage parsedData columnIndex