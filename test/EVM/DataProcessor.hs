module DataProcessor where

import Data.List (tails)

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | n > length xs = error "Window size exceeds list length"
    | otherwise = map average $ filter (\window -> length window == n) $ tails xs
  where
    average :: [Double] -> Double
    average ys = sum ys / fromIntegral (length ys)

smoothData :: [Double] -> [Double]
smoothData = movingAverage 3

main :: IO ()
main = do
    let testData = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
    putStrLn "Original data:"
    print testData
    putStrLn "\nSmoothed data (3-point moving average):"
    print $ smoothData testData
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, 0, 5, -7]
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
        in field : splitByComma (drop 1 rest)

validateNumericField :: String -> Either String Int
validateNumericField str
    | all isDigit str = Right (read str)
    | otherwise = Left $ "Invalid numeric value: " ++ str

processCSVData :: CSVData -> Either String [(String, Int)]
processCSVData rows = 
    case rows of
        [] -> Left "No data rows"
        (header:dataRows) -> 
            if length header < 2 
            then Left "Invalid header format"
            else mapM processRow dataRows
  where
    processRow :: CSVRow -> Either String (String, Int)
    processRow [name, value] = do
        validatedValue <- validateNumericField value
        return (name, validatedValue)
    processRow _ = Left "Invalid row format"

formatOutput :: [(String, Int)] -> String
formatOutput dataPairs =
    "Processed Data:\n" ++
    intercalate "\n" (map formatPair dataPairs)
  where
    formatPair (name, value) = name ++ ": " ++ show value

main :: IO ()
main = do
    let csvContent = "Name,Value\nAlice,25\nBob,30\nCharlie,invalid"
    
    putStrLn "Processing CSV data..."
    
    case parseCSV csvContent of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right csvData -> 
            case processCSVData csvData of
                Left err -> putStrLn $ "Validation error: " ++ err
                Right processed -> putStrLn $ formatOutput processedmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -1000) xs then Just xs else Nothing