
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)module DataProcessor where

movingAverage :: (Fractional a) => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
  where
    windows m ys = take (length ys - m + 1) $ zipWith (++) (tails ys) (repeat [])
    average zs = sum zs / fromIntegral (length zs)

-- Helper function from Data.List
tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = (x:xs) : tails xsmodule DataProcessor where

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
            ',' : xs -> splitByComma xs
            _        -> []
    
    trim :: String -> String
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

validateNumericField :: CSVRow -> Int -> Either String Int
validateNumericField row index
    | index < 0 || index >= length row = Left $ "Invalid column index: " ++ show index
    | otherwise = 
        let field = row !! index
        in if all isDigit field
           then Right (read field)
           else Left $ "Non-numeric value in column " ++ show index ++ ": " ++ field

calculateColumnAverage :: CSVData -> Int -> Either String Double
calculateColumnAverage rows colIndex
    | null rows = Left "No data rows"
    | otherwise = 
        case traverse (`validateNumericField` colIndex) rows of
            Left err -> Left err
            Right values -> 
                let total = sum values
                    count = length values
                in Right (fromIntegral total / fromIntegral count)

formatCSVOutput :: CSVData -> String
formatCSVOutput = intercalate "\n" . map (intercalate ",")

processCSVData :: String -> Int -> Either String (String, Double)
processCSVData input colIndex = do
    parsed <- parseCSV input
    avg <- calculateColumnAverage parsed colIndex
    return (formatCSVOutput parsed, avg)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    let result = processNumbers numbers
    print result