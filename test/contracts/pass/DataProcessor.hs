module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processNumbers input
    print resultmodule DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input = 
    if null input
    then Left "Empty input"
    else Right $ map parseRow (lines input)
  where
    parseRow :: String -> CSVRow
    parseRow = splitByComma
    
    splitByComma :: String -> [String]
    splitByComma [] = []
    splitByComma str = 
        let (field, rest) = break (== ',') str
        in trim field : case rest of
            [] -> []
            (_:xs) -> splitByComma xs
    
    trim :: String -> String
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

validateNumericField :: String -> Either String Int
validateNumericField field =
    if all isDigit trimmed
    then Right (read trimmed)
    else Left $ "Invalid numeric field: " ++ field
  where
    trimmed = trim field

processCSVData :: CSVData -> Either String [(String, Int)]
processCSVData rows =
    case rows of
        [] -> Left "No data rows"
        (_:dataRows) -> mapM processRow dataRows
  where
    processRow :: CSVRow -> Either String (String, Int)
    processRow [name, valueStr] = do
        numericValue <- validateNumericField valueStr
        return (trim name, numericValue)
    processRow row = Left $ "Invalid row format: " ++ show row

formatResults :: [(String, Int)] -> String
formatResults results =
    "Processed Results:\n" ++
    intercalate "\n" (map formatRow results) ++
    "\nTotal: " ++ show (sum $ map snd results)
  where
    formatRow (name, value) = name ++ ": " ++ show value

main :: IO ()
main = do
    let csvContent = "Name,Value\nAlice,25\nBob,30\nCharlie,invalid"
    putStrLn "Processing CSV data..."
    
    case parseCSV csvContent >>= processCSVData of
        Left err -> putStrLn $ "Error: " ++ err
        Right results -> putStrLn $ formatResults results
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles xs = sum (processNumbers xs)