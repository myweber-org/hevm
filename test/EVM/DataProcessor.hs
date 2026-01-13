module DataProcessor where

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
    | any (> 1000) xs = Nothing
    | otherwise = Just xs

safeProcess :: [Int] -> Maybe Int
safeProcess = fmap sumProcessed . validateInputmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, 0, 5, -8]
    let result = processNumbers input
    print resultmodule DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isAlpha)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> CSVData
parseCSV = map (splitOn ',') . lines
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr splitter [[]]
      where
        splitter char (current:rest)
          | char == delimiter = []:current:rest
          | otherwise = (char:current):rest

validateRow :: CSVRow -> Bool
validateRow row = length row == 3 && all validField row
  where
    validField field = not (null field) && all (\c -> isAlpha c || isDigit c || c == '-') field

filterValidRows :: CSVData -> CSVData
filterValidRows = filter validateRow

formatOutput :: CSVData -> String
formatOutput rows = intercalate "\n" $ map (intercalate " | ") rows

processCSVData :: String -> String
processCSVData input = 
  let parsed = parseCSV input
      valid = filterValidRows parsed
  in formatOutput valid

sampleData :: String
sampleData = "John,Doe,30\nJane,Smith,25\nInvalid,Data\nRobert,Johnson-2,35"