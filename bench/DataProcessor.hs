module DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter evenmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-5, 3, 0, 8, -2, 10]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter (>0)

validateData :: [Int] -> Bool
validateData xs = all (>=0) (processData xs)

sampleData :: [Int]
sampleData = [1, -2, 3, -4, 5]module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (\x -> x * 2 + 1)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

main :: IO ()
main = do
    let numbers = [1..20]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)
module DataProcessor where

import Data.List (foldl')
import Text.Read (readMaybe)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ',') (lines content)
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitHelper [[]]
      where
        splitHelper char acc@(current:rest)
          | char == delimiter = []:acc
          | otherwise = (char:current):rest

computeColumnAverages :: CSVData -> Maybe [Double]
computeColumnAverages [] = Nothing
computeColumnAverages rows@(header:_) = 
  let numericRows = map (map parseDouble) (tail rows)
      transposed = transpose numericRows
  in mapM averageColumn transposed
  where
    parseDouble :: String -> Maybe Double
    parseDouble = readMaybe
    
    transpose :: [[Maybe Double]] -> [[Maybe Double]]
    transpose [] = []
    transpose ([]:_) = []
    transpose xss = map head xss : transpose (map tail xss)
    
    averageColumn :: [Maybe Double] -> Maybe Double
    averageColumn col =
      let validValues = [x | Just x <- col]
      in if null validValues 
         then Nothing 
         else Just (sum validValues / fromIntegral (length validValues))

processCSVFile :: String -> IO ()
processCSVFile filename = do
  content <- readFile filename
  let parsed = parseCSV content
  case computeColumnAverages parsed of
    Nothing -> putStrLn "No data to process"
    Just averages -> do
      putStrLn "Column averages:"
      mapM_ (putStrLn . show) (zip [1..] averages)