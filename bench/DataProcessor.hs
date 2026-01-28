module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, 0, 5, -8]
    let result = processNumbers input
    print resultmodule DataProcessor where

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

safeReadDouble :: String -> Maybe Double
safeReadDouble = readMaybe

computeColumnStats :: CSVData -> Int -> Maybe (Double, Double, Double)
computeColumnStats rows columnIndex
  | null rows = Nothing
  | otherwise = do
      let values = mapMaybe (safeReadDouble . (!! columnIndex)) rows
      if null values
        then Nothing
        else let sumVal = foldl' (+) 0 values
                 count = fromIntegral (length values)
                 mean = sumVal / count
                 variance = foldl' (\acc x -> acc + (x - mean)^2) 0 values / count
             in Just (mean, variance, sqrt variance)

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = foldr (\x acc -> case f x of
                                Just val -> val:acc
                                Nothing -> acc) []

processCSVFile :: String -> String -> IO ()
processCSVFile inputFile outputFile = do
  content <- readFile inputFile
  let csvData = parseCSV content
  case csvData of
    [] -> putStrLn "Empty CSV file"
    (header:rows) -> do
      let stats = map (\i -> computeColumnStats rows i) [0..length header - 1]
      let results = zipWith (\h s -> (h, s)) header stats
      writeFile outputFile (formatResults results)
      putStrLn $ "Statistics written to " ++ outputFile

formatResults :: [(String, Maybe (Double, Double, Double))] -> String
formatResults = unlines . map formatRow
  where
    formatRow (colName, Nothing) = colName ++ ": No numeric data"
    formatRow (colName, Just (mean, variance, stdDev)) =
      colName ++ ": Mean=" ++ show mean ++ ", Variance=" ++ show variance ++ ", StdDev=" ++ show stdDevmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-5, 3, 0, 8, -2, 10]
    let result = processNumbers numbers
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)