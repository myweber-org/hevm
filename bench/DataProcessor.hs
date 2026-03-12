
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessed :: (Int -> Int) -> [Int] -> Int
sumProcessed processor = sum . map processor

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Even squares: " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of doubled values: " ++ show (sumProcessed (*2) numbers)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

main :: IO ()
main = do
    let numbers = [-3, 1, 4, -1, 5, 9, -2, 6]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)
module DataProcessor where

import Data.List (intercalate)
import Text.Read (readMaybe)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ',') (lines content)
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitHelper [[]]
      where
        splitHelper char (current:rest)
          | char == delimiter = []:current:rest
          | otherwise = (char:current):rest

calculateColumnAverages :: CSVData -> Maybe [Double]
calculateColumnAverages [] = Nothing
calculateColumnAverages rows@(header:_) = 
  let numericRows = map (map readMaybe) (tail rows)
      transposed = transpose numericRows
      columnSums = map (foldl addMaybes (Just 0)) transposed
      columnCounts = map (foldl countMaybes 0) transposed
  in sequence $ zipWith avgHelper columnSums columnCounts
  where
    transpose :: [[Maybe a]] -> [[Maybe a]]
    transpose [] = []
    transpose ([]:_) = []
    transpose xss = map head xss : transpose (map tail xss)
    
    addMaybes :: Maybe Double -> Maybe Double -> Maybe Double
    addMaybes (Just x) (Just y) = Just (x + y)
    addMaybes _ _ = Nothing
    
    countMaybes :: Int -> Maybe a -> Int
    countMaybes count (Just _) = count + 1
    countMaybes count Nothing = count
    
    avgHelper :: Maybe Double -> Int -> Maybe Double
    avgHelper (Just sum) count
      | count > 0 = Just (sum / fromIntegral count)
      | otherwise = Nothing
    avgHelper _ _ = Nothing

formatAverages :: [Double] -> String
formatAverages averages = 
  "Column averages:\n" ++ 
  intercalate "\n" (zipWith formatLine [1..] averages)
  where
    formatLine idx avg = 
      "Column " ++ show idx ++ ": " ++ printf "%.2f" avg
    
    printf :: String -> Double -> String
    printf fmt val = 
      let (intPart, fracPart) = properFraction val
          rounded = fromIntegral (round (val * 100)) / 100
      in show rounded

processCSVFile :: String -> IO ()
processCSVFile filename = do
  content <- readFile filename
  let parsed = parseCSV content
  case calculateColumnAverages parsed of
    Just averages -> putStrLn $ formatAverages averages
    Nothing -> putStrLn "No data to process or invalid format"