
module DataProcessor where

import Data.List (intercalate)
import Data.Char (isDigit)

data ValidationError = InvalidFormat String | MissingField String | InvalidValue String
    deriving (Show, Eq)

type CSVRow = [String]
type ValidatedRow = Either ValidationError CSVRow

validateRow :: CSVRow -> ValidatedRow
validateRow [] = Left (MissingField "Empty row")
validateRow [idStr, name, ageStr] =
    if all isDigit idStr && all isDigit ageStr
        then if read ageStr > 0 && read ageStr < 150
            then Right [idStr, name, ageStr]
            else Left (InvalidValue $ "Invalid age: " ++ ageStr)
        else Left (InvalidFormat "ID and age must be numeric")
validateRow _ = Left (InvalidFormat "Row must have exactly 3 columns")

parseCSV :: String -> [ValidatedRow]
parseCSV = map (validateRow . splitByComma) . lines
    where
        splitByComma = foldr splitter [[]]
        splitter ',' (x:xs) = []:x:xs
        splitter '"' (x:xs) = reverse x:xs
        splitter c (x:xs) = (c:x):xs

formatResults :: [ValidatedRow] -> String
formatResults rows = intercalate "\n" $ map formatRow (zip [1..] rows)
    where
        formatRow (n, Left err) = "Row " ++ show n ++ ": ERROR - " ++ show err
        formatRow (n, Right vals) = "Row " ++ show n ++ ": OK - " ++ intercalate ", " vals

processCSVData :: String -> String
processCSVData = formatResults . parseCSV
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    print result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processEvenSquares

validateInput :: [Int] -> Maybe [Int]
validateInput xs
    | null xs = Nothing
    | any (< 0) xs = Nothing
    | otherwise = Just xs

main :: IO ()
main = do
    let sampleData = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    case validateInput sampleData of
        Just validData -> do
            putStrLn $ "Original data: " ++ show validData
            putStrLn $ "Processed data: " ++ show (processEvenSquares validData)
            putStrLn $ "Sum of processed data: " ++ show (sumProcessedData validData)
        Nothing -> putStrLn "Invalid input data"