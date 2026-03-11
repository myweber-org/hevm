module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

import Data.List.Split (splitOn)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ",") (lines content)

extractNumericColumn :: CSVData -> Int -> [Double]
extractNumericColumn rows colIndex = 
    [ read cell | row <- rows, 
                  length row > colIndex, 
                  let cell = row !! colIndex, 
                  all (\c -> c `elem` "0123456789.-") cell ]

calculateAverage :: [Double] -> Double
calculateAverage [] = 0.0
calculateAverage xs = sum xs / fromIntegral (length xs)

processCSVFile :: String -> Int -> IO Double
processCSVFile filePath columnIndex = do
    content <- readFile filePath
    let parsedData = parseCSV content
    let numericData = extractNumericColumn parsedData columnIndex
    return $ calculateAverage numericDatamodule DataProcessor where

processData :: [Int] -> [Int]
processData xs = map (^2) (filter even xs)module DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter (>0)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbersmodule DataProcessor where

import Data.List (sort, group, maximumBy)
import Data.Ord (comparing)
import qualified Data.Map as Map

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> CSVData
parseCSV = map (splitOn ',') . lines
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitHelper [""]
      where
        splitHelper char acc@(current:rest)
          | char == delimiter = "":acc
          | otherwise = (char:current):rest

calculateMean :: [Double] -> Maybe Double
calculateMean [] = Nothing
calculateMean xs = Just (sum xs / fromIntegral (length xs))

calculateMedian :: [Double] -> Maybe Double
calculateMedian [] = Nothing
calculateMedian xs =
  let sorted = sort xs
      len = length sorted
      mid = len `div` 2
  in if odd len
     then Just (sorted !! mid)
     else Just ((sorted !! (mid - 1) + sorted !! mid) / 2)

calculateMode :: (Ord a) => [a] -> Maybe a
calculateMode [] = Nothing
calculateMode xs =
  let frequencyMap = Map.fromListWith (+) [(x, 1) | x <- xs]
  in case Map.maxViewWithKey frequencyMap of
       Just ((mode, _), _) -> Just mode
       Nothing -> Nothing

filterRowsByColumn :: Int -> (String -> Bool) -> CSVData -> CSVData
filterRowsByColumn colIndex predicate csvData =
  filter (\row -> if length row > colIndex then predicate (row !! colIndex) else False) csvData

extractNumericColumn :: Int -> CSVData -> [Double]
extractNumericColumn colIndex csvData =
  [read value | row <- csvData, length row > colIndex, 
   let value = row !! colIndex, all (`elem` "0123456789.-") value]

processCSVFile :: FilePath -> IO ()
processCSVFile filePath = do
  content <- readFile filePath
  let csvData = parseCSV content
  putStrLn $ "Total rows: " ++ show (length csvData)
  
  case csvData of
    [] -> putStrLn "Empty CSV file"
    (header:rows) -> do
      putStrLn $ "Columns: " ++ show (length header)
      putStrLn "Header: " ++ show header
      
      let numericData = extractNumericColumn 1 rows
      putStrLn $ "Numeric values in column 1: " ++ show (length numericData)
      
      case calculateMean numericData of
        Just mean -> putStrLn $ "Mean: " ++ show mean
        Nothing -> putStrLn "Cannot calculate mean"
      
      case calculateMedian numericData of
        Just median -> putStrLn $ "Median: " ++ show median
        Nothing -> putStrLn "Cannot calculate median"
      
      case calculateMode numericData of
        Just mode -> putStrLn $ "Mode: " ++ show mode
        Nothing -> putStrLn "Cannot calculate mode"
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processData