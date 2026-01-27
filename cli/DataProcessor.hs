
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedList :: [Int] -> Int
sumProcessedList = sum . processEvenSquares

main :: IO ()
main = do
    let sampleData = [1..10]
    let result = sumProcessedList sampleData
    putStrLn $ "Sum of squares of even numbers from 1 to 10: " ++ show resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, 0, 5, -8]
    let result = processNumbers input
    print resultmodule DataProcessor where

import Data.List.Split (splitOn)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ",") (lines content)

extractNumericColumn :: CSVData -> Int -> Maybe [Double]
extractNumericColumn rows colIndex
    | null rows = Nothing
    | otherwise = sequence $ map (safeRead . (!! colIndex)) rows
  where
    safeRead str = case reads str of
        [(x, "")] -> Just x
        _ -> Nothing

calculateAverage :: [Double] -> Maybe Double
calculateAverage xs
    | null xs = Nothing
    | otherwise = Just (sum xs / fromIntegral (length xs))

processCSVFile :: String -> Int -> IO (Maybe Double)
processCSVFile filePath columnIndex = do
    content <- readFile filePath
    let parsedData = parseCSV content
    case extractNumericColumn parsedData columnIndex of
        Nothing -> return Nothing
        Just values -> return $ calculateAverage values