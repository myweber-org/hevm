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
    putStrLn $ "Processed: " ++ show resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processNumbers input
    print result
module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type CSVRow = [String]
type ValidatedRow = Either String [Int]

parseCSV :: String -> [CSVRow]
parseCSV content = map (splitOn ',') (lines content)
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitHelper [""]
      where
        splitHelper ch (x:xs)
          | ch == delimiter = "":x:xs
          | otherwise = (ch:x):xs

validateRow :: CSVRow -> ValidatedRow
validateRow row
  | length row /= 3 = Left "Row must contain exactly 3 fields"
  | any null row   = Left "Empty field detected"
  | otherwise      = mapM validateField row
  where
    validateField :: String -> Either String Int
    validateField s
      | all isDigit s = Right (read s)
      | otherwise     = Left $ "Non-numeric value: " ++ s

processCSVData :: String -> Either String [[Int]]
processCSVData content = do
  let rows = parseCSV content
  mapM validateRow rows

formatOutput :: [[Int]] -> String
formatOutput rows = intercalate "\n" (map formatRow rows)
  where
    formatRow :: [Int] -> String
    formatRow [a,b,c] = show a ++ "," ++ show b ++ "," ++ show c
    formatRow _ = error "Invalid row length"

main :: IO ()
main = do
  let sampleData = "1,2,3\n4,5,6\n7,8,9"
  case processCSVData sampleData of
    Left err -> putStrLn $ "Error: " ++ err
    Right validated -> putStrLn $ "Processed data:\n" ++ formatOutput validatedmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print resultmodule DataProcessor where

import Data.List.Split (splitOn)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ",") (lines content)

numericColumns :: CSVData -> [Int]
numericColumns [] = []
numericColumns (header:_) = 
    map fst $ filter (\(_, h) -> h == "numeric") $ zip [0..] header

calculateColumnAverage :: CSVData -> Int -> Maybe Double
calculateColumnAverage rows colIndex
    | null numericRows = Nothing
    | otherwise = Just (sum numericValues / fromIntegral (length numericValues))
  where
    numericRows = filter (\row -> 
        length row > colIndex && 
        all (\c -> c `elem` "0123456789.") (row !! colIndex)) rows
    numericValues = map (read . (!! colIndex)) numericRows

processCSVFile :: String -> IO ()
processCSVFile filename = do
    content <- readFile filename
    let csvData = parseCSV content
    let numericCols = numericColumns csvData
    
    putStrLn "Column averages:"
    mapM_ (\col -> 
        case calculateColumnAverage csvData col of
            Just avg -> putStrLn $ "Column " ++ show col ++ ": " ++ show avg
            Nothing -> putStrLn $ "Column " ++ show col ++ ": No numeric data"
        ) numericCols
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedList :: [Int] -> Int
sumProcessedList = sum . processEvenSquares

main :: IO ()
main = do
    let sampleData = [1..10]
    let result = sumProcessedList sampleData
    putStrLn $ "Sum of squares of even numbers from 1 to 10: " ++ show result