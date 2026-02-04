module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -100) xs then Just xs else Nothingmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -100) xs then Just xs else Nothing
module DataProcessor where

import Data.List.Split (splitOn)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ",") (lines content)

numericColumns :: CSVData -> [Int]
numericColumns [] = []
numericColumns (header:rows) = 
    [i | (i, cell) <- zip [0..] header, all isNumeric (map (!! i) rows)]
  where
    isNumeric str = case reads str :: [(Double, String)] of
        [(_, "")] -> True
        _ -> False

columnAverage :: CSVData -> Int -> Maybe Double
columnAverage rows colIndex
    | null numericValues = Nothing
    | otherwise = Just (sum numericValues / fromIntegral (length numericValues))
  where
    numericValues = [read cell :: Double | row <- rows, 
                    colIndex < length row,
                    let cell = row !! colIndex,
                    case reads cell :: [(Double, String)] of
                        [(_, "")] -> True
                        _ -> False]

processCSVFile :: String -> IO ()
processCSVFile filename = do
    content <- readFile filename
    let parsed = parseCSV content
    case parsed of
        [] -> putStrLn "Empty CSV file"
        (header:dataRows) -> do
            putStrLn $ "Header: " ++ show header
            let numericCols = numericColumns parsed
            putStrLn $ "Numeric columns: " ++ show numericCols
            mapM_ (\col -> 
                case columnAverage dataRows col of
                    Just avg -> putStrLn $ "Column " ++ show col ++ 
                                " (" ++ (header !! col) ++ 
                                ") average: " ++ show avg
                    Nothing -> putStrLn $ "Column " ++ show col ++ 
                               " has no numeric data"
                ) numericCols