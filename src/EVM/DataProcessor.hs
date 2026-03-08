module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit, isAlpha)

type CSVRow = [String]
type ValidationError = String

validateRow :: CSVRow -> Either ValidationError CSVRow
validateRow row
    | length row /= 3 = Left "Row must contain exactly 3 columns"
    | not (all validName [row !! 0]) = Left "First column must contain only letters"
    | not (all isDigit (row !! 1)) = Left "Second column must contain only digits"
    | not (validAge (row !! 2)) = Left "Third column must be a valid age (1-150)"
    | otherwise = Right row
  where
    validName = all isAlpha
    validAge ageStr = 
        case reads ageStr :: [(Int, String)] of
            [(age, "")] -> age >= 1 && age <= 150
            _ -> False

parseCSV :: String -> Either ValidationError [CSVRow]
parseCSV content = 
    let rows = map (splitOn ',') (lines content)
        validatedRows = map validateRow rows
    in case partitionEithers validatedRows of
        ([], validRows) -> Right validRows
        (errors, _) -> Left (intercalate "; " errors)

splitOn :: Char -> String -> [String]
splitOn delimiter = foldr splitHelper [""]
  where
    splitHelper char acc
        | char == delimiter = "":acc
        | otherwise = (char:head acc):tail acc

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr select ([], [])
  where
    select (Left a) (as, bs) = (a:as, bs)
    select (Right b) (as, bs) = (as, b:bs)

processCSVData :: String -> IO ()
processCSVData filename = do
    content <- readFile filename
    case parseCSV content of
        Left err -> putStrLn $ "Validation error: " ++ err
        Right rows -> do
            putStrLn "Valid CSV data:"
            mapM_ (putStrLn . intercalate " | ") rows
            let total = length rows
            putStrLn $ "Total valid records: " ++ show totalmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    let result = processNumbers numbers
    print result