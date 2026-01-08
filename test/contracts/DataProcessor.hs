module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print resultmodule DataProcessor where

import Data.List (foldl')
import Text.CSV (parseCSV)

type Row = [String]
type CSVData = [Row]

parseCSVData :: String -> Either String CSVData
parseCSVData input = case parseCSV "input" input of
    Left err -> Left $ "Parse error: " ++ err
    Right csv -> Right csv

computeColumnAverage :: CSVData -> Int -> Either String Double
computeColumnAverage [] _ = Left "Empty data"
computeColumnAverage rows colIndex
    | colIndex < 0 = Left "Column index cannot be negative"
    | otherwise = case validNumbers of
        [] -> Left "No valid numeric data in column"
        nums -> Right $ sum nums / fromIntegral (length nums)
  where
    extractNumbers = mapMaybe (safeRead . (!! colIndex)) rows
    validNumbers = filter (not . isNaN) extractNumbers

    safeRead :: String -> Maybe Double
    safeRead s = case reads s of
        [(x, "")] -> Just x
        _ -> Nothing

    mapMaybe :: (a -> Maybe b) -> [a] -> [b]
    mapMaybe _ [] = []
    mapMaybe f (x:xs) = case f x of
        Just y -> y : mapMaybe f xs
        Nothing -> mapMaybe f xs

processCSVFile :: String -> Int -> IO ()
processCSVFile filename colIndex = do
    content <- readFile filename
    case parseCSVData content of
        Left err -> putStrLn $ "Error: " ++ err
        Right csvData -> case computeColumnAverage csvData colIndex of
            Left err -> putStrLn $ "Error: " ++ err
            Right avg -> putStrLn $ "Average of column " ++ show colIndex ++ ": " ++ show avg