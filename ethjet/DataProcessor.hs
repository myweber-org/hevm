module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -100) xs then Just xs else Nothing

main :: IO ()
main = do
    let sampleData = [1, -5, 3, 0, 8, -2]
    case validateInput sampleData of
        Just validData -> do
            putStrLn $ "Original: " ++ show validData
            putStrLn $ "Processed: " ++ show (processNumbers validData)
            putStrLn $ "Sum: " ++ show (sumProcessed validData)
        Nothing -> putStrLn "Invalid input detected"
module DataProcessor where

import Data.List (foldl')
import Text.Read (readMaybe)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV = map (splitOn ',') . lines
  where
    splitOn :: Char -> String -> [String]
    splitOn delim = foldr splitHelper [""]
      where
        splitHelper ch (x:xs)
          | ch == delim = "":x:xs
          | otherwise = (ch:x):xs

calculateColumnAverage :: CSVData -> Int -> Maybe Double
calculateColumnAverage [] _ = Nothing
calculateColumnAverage rows colIndex
  | colIndex < 0 = Nothing
  | otherwise = case validNumbers of
      [] -> Nothing
      nums -> Just (sum nums / fromIntegral (length nums))
  where
    extractNumbers = mapMaybe (\row -> 
      if colIndex < length row 
        then readMaybe (row !! colIndex) :: Maybe Double
        else Nothing) rows
    
    validNumbers = filter (not . isNaN) extractNumbers

processCSVFile :: FilePath -> Int -> IO (Maybe Double)
processCSVFile filePath column = do
  content <- readFile filePath
  let parsed = parseCSV content
  return $ calculateColumnAverage parsed column

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

validateCSV :: CSVData -> Bool
validateCSV [] = True
validateCSV (row:rows) = 
  let rowLength = length row
  in all ((== rowLength) . length) rows