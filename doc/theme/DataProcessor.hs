module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)module DataProcessor where

import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV = map (splitOn ',') . lines
  where
    splitOn :: Char -> String -> [String]
    splitOn delim = foldr splitHelper [[]]
      where
        splitHelper ch (x:xs)
          | ch == delim = []:x:xs
          | otherwise = (ch:x):xs

safeReadDouble :: String -> Maybe Double
safeReadDouble = readMaybe

computeColumnStats :: CSVData -> [String] -> [(Double, Double, Double)]
computeColumnStats rows columnNames =
  let header = head rows
      dataRows = tail rows
      colIndices = map (\name -> elemIndex name header) columnNames
      getColumnData idx = mapMaybe (safeReadDouble . (!! idx)) dataRows
      columnData = map (getColumnData . fromMaybe 0) colIndices
      stats dataList =
        let count = fromIntegral $ length dataList
            sumVal = foldl' (+) 0 dataList
            mean = sumVal / count
            variance = foldl' (\acc x -> acc + (x - mean)^2) 0 dataList / count
            stdDev = sqrt variance
        in (mean, variance, stdDev)
  in map stats columnData

processCSVFile :: FilePath -> [String] -> IO ()
processCSVFile filePath columns = do
  contents <- readFile filePath
  let csvData = parseCSV contents
      results = computeColumnStats csvData columns
  mapM_ (\(mean, var, std) ->
    putStrLn $ "Mean: " ++ show mean ++
               ", Variance: " ++ show var ++
               ", StdDev: " ++ show std) results
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processDatamodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers