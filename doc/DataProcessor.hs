module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> Either String CSVData
parseCSV input = mapM parseRow (lines input)
  where
    parseRow line = case splitOnComma line of
        [] -> Left "Empty row"
        cells -> Right (map trim cells)

splitOnComma :: String -> [String]
splitOnComma [] = []
splitOnComma str = 
    let (cell, rest) = break (== ',') str
    in cell : case rest of
                ',' : xs -> splitOnComma xs
                _        -> []

trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

validateRow :: Row -> Either String Row
validateRow [] = Left "Row cannot be empty"
validateRow cells
    | any null cells = Left "Empty cell detected"
    | length cells /= expectedColumns = Left $ "Expected " ++ show expectedColumns ++ " columns"
    | not (all isValidNumeric (take 2 cells)) = Left "First two cells must be numeric"
    | otherwise = Right cells
  where
    expectedColumns = 4
    isValidNumeric = all isDigit

processCSV :: String -> Either String CSVData
processCSV input = do
    parsed <- parseCSV input
    mapM validateRow parsed

formatOutput :: CSVData -> String
formatOutput rows = 
    intercalate "\n" (map formatRow rows)
  where
    formatRow cells = intercalate " | " cells

main :: IO ()
main = do
    let sampleData = "123,456,Product A,Active\n789,012,Product B,Inactive"
    case processCSV sampleData of
        Left err -> putStrLn $ "Error: " ++ err
        Right data' -> putStrLn $ "Processed data:\n" ++ formatOutput data'module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
  where
    windows m ys = take (length ys - m + 1) $ zipWith (++) (tails ys) (repeat [])
    average zs = sum zs / fromIntegral (length zs)
module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isAlpha)
import Control.Monad (foldM)

type CSVRow = [String]
type CSVData = [CSVRow]

data ValidationError = 
    EmptyRowError Int
  | InvalidColumnCountError Int Int Int
  | InvalidNumericFieldError Int Int String
  | InvalidAlphaFieldError Int Int String
  deriving (Show, Eq)

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ',') (lines content)
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitHelper [""]
      where
        splitHelper ch acc@(current:rest)
          | ch == delimiter = "":acc
          | otherwise = (ch:current):rest

validateCSVData :: CSVData -> Either [ValidationError] CSVData
validateCSVData rows = case errors of
  [] -> Right rows
  _  -> Left errors
  where
    errors = concatMap validateRow (zip [1..] rows)
    
    validateRow :: (Int, CSVRow) -> [ValidationError]
    validateRow (rowNum, row)
      | null row = [EmptyRowError rowNum]
      | length row /= expectedColumns = [InvalidColumnCountError rowNum (length row) expectedColumns]
      | otherwise = concat $ zipWith (validateField rowNum) [1..] row
    
    expectedColumns = 4
    
    validateField :: Int -> Int -> String -> [ValidationError]
    validateField rowNum colNum value
      | colNum == 1 && not (all isAlpha value) = [InvalidAlphaFieldError rowNum colNum value]
      | colNum == 2 && not (all isDigit value) = [InvalidNumericFieldError rowNum colNum value]
      | colNum == 3 && not (all isDigit value) = [InvalidNumericFieldError rowNum colNum value]
      | colNum == 4 && not (all isAlpha value) = [InvalidAlphaFieldError rowNum colNum value]
      | otherwise = []

processValidatedData :: CSVData -> (Int, Int, [String])
processValidatedData rows = (totalCount, numericSum, names)
  where
    totalCount = length rows
    numericSum = sum $ map (read . (!!1)) rows
    names = map head rows

formatReport :: (Int, Int, [String]) -> String
formatReport (count, sum, names) = 
  "Total records: " ++ show count ++ "\n" ++
  "Sum of second column: " ++ show sum ++ "\n" ++
  "Names: " ++ intercalate ", " names

main :: IO ()
main = do
  let sampleData = "John,25,100,USA\nAlice,30,200,UK\nBob,35,150,Canada"
  let parsed = parseCSV sampleData
  case validateCSVData parsed of
    Left errs -> putStrLn $ "Validation errors:\n" ++ unlines (map show errs)
    Right validData -> do
      let result = processValidatedData validData
      putStrLn $ formatReport resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (*2)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)
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
    putStrLn $ "Sum of doubled values: " ++ show (sumProcessed (*2) numbers)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processNumbers input
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (*2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers