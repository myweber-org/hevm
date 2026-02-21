
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateAndProcess :: [Int] -> Maybe [Int]
validateAndProcess xs
    | null xs = Nothing
    | otherwise = Just $ processData xsmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, 0, 5, -8]
    let result = processData input
    print result
module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isSpace)
import Control.Monad (when)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input
  | null input = Right []
  | otherwise = mapM parseRow (lines input)
  where
    parseRow :: String -> Either String CSVRow
    parseRow line = case parseFields line [] "" False of
      Left err -> Left err
      Right fields -> Right (reverse fields)
    
    parseFields :: String -> [String] -> String -> Bool -> Either String [String]
    parseFields [] acc current inQuotes
      | inQuotes = Left "Unclosed quote in CSV"
      | otherwise = Right (reverse (if null current then acc else current:acc))
    
    parseFields (c:cs) acc current inQuotes
      | c == '"' = if inQuotes
                   then if not (null cs) && head cs == '"'
                        then parseFields (tail cs) acc (current ++ [c]) True
                        else parseFields cs acc current False
                   else parseFields cs acc current True
      | not inQuotes && c == ',' = parseFields cs (current:acc) "" False
      | not inQuotes && isSpace c = parseFields cs acc current False
      | otherwise = parseFields cs acc (current ++ [c]) inQuotes

validateNumericColumn :: CSVData -> Int -> Either String ()
validateNumericColumn [] _ = Right ()
validateNumericColumn (header:rows) colIndex
  | colIndex < 0 || colIndex >= length header = Left "Column index out of bounds"
  | otherwise = validateRows rows colIndex 1
  where
    validateRows [] _ _ = Right ()
    validateRows (row:rest) idx lineNum
      | idx >= length row = Left $ "Row " ++ show lineNum ++ ": missing column " ++ show idx
      | not (all isDigit (filter (/= '.') (row !! idx))) = 
          Left $ "Row " ++ show lineNum ++ ": column " ++ show idx ++ " contains non-numeric value"
      | otherwise = validateRows rest idx (lineNum + 1)

processCSVData :: String -> Int -> Either String CSVData
processCSVData input colIndex = do
  csvData <- parseCSV input
  when (null csvData) (Left "Empty CSV data")
  validateNumericColumn csvData colIndex
  return csvData

formatCSVOutput :: CSVData -> String
formatCSVOutput = intercalate "\n" . map (intercalate ",")

main :: IO ()
main = do
  let testData = "Name,Age,Score\nJohn,25,95.5\nAlice,thirty,88.0\nBob,30,92.3"
  case processCSVData testData 1 of
    Left err -> putStrLn $ "Error: " ++ err
    Right data -> putStrLn $ "Valid CSV:\n" ++ formatCSVOutput datamodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let sampleData = [1..10]
    let result = processData sampleData
    print resultmodule DataProcessor where

movingAverage :: (Fractional a) => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
  where
    windows m ys = take (length ys - m + 1) $ zipWith (++) (tails ys) (repeat [])
    average zs = sum zs / fromIntegral (length zs)

-- Helper function similar to Data.List.tails
tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = (x:xs) : tails xsmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs
    | null xs = Nothing
    | any (< -100) xs = Nothing
    | any (> 100) xs = Nothing
    | otherwise = Just xsmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData xs = all (> 0) xs && length xs > 3

combineResults :: [Int] -> [Int] -> [Int]
combineResults xs ys = zipWith (+) (processData xs) (processData ys)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData