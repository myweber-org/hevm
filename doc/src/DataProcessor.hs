module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Bool
validateInput xs = not (null xs) && all (> -100) xs

main :: IO ()
main = do
    let sampleData = [1, -5, 3, 0, 8, -2]
    if validateInput sampleData
        then do
            putStrLn $ "Original data: " ++ show sampleData
            putStrLn $ "Processed data: " ++ show (processData sampleData)
            putStrLn $ "Sum of processed data: " ++ show (sumProcessedData sampleData)
        else putStrLn "Invalid input data"module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform isEven square
    where
        isEven x = x `mod` 2 == 0
        square x = x * x

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

import Data.List (foldl')
import Text.Read (readMaybe)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV = map (splitOn ',') . lines
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitHelper [""]
      where
        splitHelper :: Char -> [String] -> [String]
        splitHelper c (x:xs)
          | c == delimiter = "":x:xs
          | otherwise = (c:x):xs

calculateColumnAverages :: CSVData -> Maybe [Double]
calculateColumnAverages [] = Nothing
calculateColumnAverages rows@(header:_) = 
  let numericRows = filter (all isNumeric) (tail rows)
      columnCount = length header
  in if null numericRows 
     then Nothing
     else Just $ map (calculateAverage columnCount) [0..columnCount-1]
  where
    isNumeric :: String -> Bool
    isNumeric str = case readMaybe str :: Maybe Double of
      Just _ -> True
      Nothing -> False
    
    calculateAverage :: Int -> Int -> Double
    calculateAverage colCount colIndex =
      let columnValues = map (\[email protected](x:xs) -> 
            if length row > colIndex 
            then read (row !! colIndex) 
            else 0.0) numericRows
          total = foldl' (+) 0.0 columnValues
      in total / fromIntegral (length columnValues)

processCSVFile :: FilePath -> IO (Maybe [Double])
processCSVFile path = do
  content <- readFile path
  let parsed = parseCSV content
  return $ calculateColumnAverages parsed

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x