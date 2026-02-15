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

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

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
    isNumericChar c = c `elem` ['0'..'9'] || c == '.'

columnAverage :: CSVData -> Int -> Maybe Double
columnAverage rows colIndex
    | null numericValues = Nothing
    | otherwise = Just (sum numericValues / fromIntegral (length numericValues))
  where
    numericValues = [read cell | row <- rows, 
                   length row > colIndex,
                   let cell = row !! colIndex,
                   all (\c -> c `elem` ['0'..'9'] || c == '.') cell]

processCSVFile :: String -> IO ()
processCSVFile filename = do
    content <- readFile filename
    let parsed = parseCSV content
    let numericCols = numericColumns parsed
    
    putStrLn "Column averages:"
    mapM_ (\col -> 
        case columnAverage (tail parsed) col of
            Just avg -> putStrLn $ "Column " ++ show col ++ ": " ++ show avg
            Nothing -> putStrLn $ "Column " ++ show col ++ ": No numeric data"
        ) numericCols