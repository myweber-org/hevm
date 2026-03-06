module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (* 2)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -100) xs then Just xs else Nothingmodule DataProcessor where

import Data.List.Split (splitOn)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ",") (lines content)

numericColumns :: CSVData -> [Int]
numericColumns [] = []
numericColumns (header:_) = 
    [i | (i, cell) <- zip [0..] header, all isNumericChar cell]
  where
    isNumericChar c = c `elem` "0123456789.-"

columnAverage :: CSVData -> Int -> Maybe Double
columnAverage rows colIndex
    | null numericValues = Nothing
    | otherwise = Just (sum numericValues / fromIntegral (length numericValues))
  where
    numericValues = [read cell | row <- rows, 
                   length row > colIndex,
                   let cell = row !! colIndex,
                   all (\c -> c `elem` "0123456789.-") cell]

processCSVFile :: String -> IO ()
processCSVFile filename = do
    content <- readFile filename
    let dataRows = parseCSV content
    case dataRows of
        [] -> putStrLn "Empty CSV file"
        (header:rows) -> do
            putStrLn $ "Processing CSV with columns: " ++ unwords header
            let numCols = numericColumns dataRows
            mapM_ (\col -> 
                case columnAverage (header:rows) col of
                    Just avg -> putStrLn $ 
                        "Average for column " ++ show col ++ 
                        " (" ++ header !! col ++ "): " ++ show avg
                    Nothing -> putStrLn $
                        "No numeric data in column " ++ show col
                ) numColsmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData