module DataProcessor where

import Data.List.Split (splitOn)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ",") (lines content)

numericColumns :: CSVData -> [Int]
numericColumns [] = []
numericColumns (header:rows) = 
    [i | i <- [0..length header - 1], 
         all (isNumeric . (!! i)) rows]
  where
    isNumeric str = case reads str :: [(Double, String)] of
        [(_, "")] -> True
        _         -> False

calculateColumnAverage :: CSVData -> Int -> Maybe Double
calculateColumnAverage rows colIndex
    | null validRows = Nothing
    | otherwise = Just (sum validRows / fromIntegral (length validRows))
  where
    validRows = [read (row !! colIndex) | row <- rows, 
                colIndex < length row,
                case reads (row !! colIndex) :: [(Double, String)] of
                    [(val, "")] -> True
                    _           -> False]

processCSVFile :: String -> IO ()
processCSVFile filename = do
    content <- readFile filename
    let parsed = parseCSV content
    case parsed of
        [] -> putStrLn "Empty CSV file"
        (header:rows) -> do
            putStrLn $ "Processing file: " ++ filename
            putStrLn $ "Total rows: " ++ show (length rows)
            
            let numericCols = numericColumns parsed
            putStrLn $ "Numeric columns: " ++ show numericCols
            
            mapM_ (\col -> do
                case calculateColumnAverage rows col of
                    Just avg -> putStrLn $ 
                        "Column " ++ show col ++ 
                        " (" ++ (header !! col) ++ 
                        ") average: " ++ show avg
                    Nothing -> putStrLn $ 
                        "Column " ++ show col ++ 
                        " has no numeric data"
                ) numericColsmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)