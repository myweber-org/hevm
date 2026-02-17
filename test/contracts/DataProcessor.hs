module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isSpace)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> Either String CSVData
parseCSV input = case lines input of
    [] -> Left "Empty input"
    rows -> traverse parseRow rows
  where
    parseRow :: String -> Either String CSVRow
    parseRow row = case parseRow' row [] "" False of
        Right fields -> Right $ reverse fields
        Left err -> Left err

    parseRow' :: String -> [String] -> String -> Bool -> Either String [String]
    parseRow' [] acc current _ = Right $ reverse (current : acc)
    parseRow' (c:cs) acc current quoted
        | c == '"' && not quoted = parseRow' cs acc current True
        | c == '"' && quoted = parseRow' cs acc current False
        | c == ',' && not quoted = parseRow' cs (current : acc) "" False
        | quoted = parseRow' cs acc (c : current) True
        | isSpace c = parseRow' cs acc current False
        | otherwise = parseRow' cs acc (c : current) False

validateNumericField :: String -> Either String Int
validateNumericField str
    | all isDigit str = Right $ read str
    | otherwise = Left $ "Invalid numeric value: " ++ str

processCSVData :: CSVData -> Either String [(String, Int)]
processCSVData [] = Left "No data to process"
processCSVData rows = traverse processRow rows
  where
    processRow :: CSVRow -> Either String (String, Int)
    processRow [name, valueStr] = do
        validatedValue <- validateNumericField valueStr
        return (trim name, validatedValue)
    processRow row = Left $ "Invalid row format: " ++ show row

    trim :: String -> String
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

formatOutput :: [(String, Int)] -> String
formatOutput dataPairs = intercalate "\n" $ map formatPair dataPairs
  where
    formatPair (name, value) = name ++ ": " ++ show value

main :: IO ()
main = do
    let csvContent = "Alice,25\nBob,30\nCharlie,abc\nDavid,40"
    putStrLn "Original CSV:"
    putStrLn csvContent
    putStrLn "\nProcessing results:"
    
    case parseCSV csvContent of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right parsed -> case processCSVData parsed of
            Left err -> putStrLn $ "Validation error: " ++ err
            Right processed -> do
                putStrLn "Validated data:"
                putStrLn $ formatOutput processed
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (\x -> x >= -100 && x <= 100) xs
                   then Just xs
                   else Nothing

main :: IO ()
main = do
    let sampleData = [-5, 2, 0, 8, -3, 10]
    case validateInput sampleData of
        Just validData -> do
            let result = processData validData
            putStrLn $ "Original: " ++ show validData
            putStrLn $ "Processed: " ++ show result
        Nothing -> putStrLn "Input validation failed: values must be between -100 and 100"module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)