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

columnAverage :: CSVData -> Int -> Maybe Double
columnAverage rows colIndex
    | null numericValues = Nothing
    | otherwise = Just (sum numericValues / fromIntegral (length numericValues))
  where
    numericValues = [read x :: Double | row <- rows, 
                                        length row > colIndex,
                                        let val = row !! colIndex,
                                        isNumeric val]
    isNumeric str = case reads str :: [(Double, String)] of
        [(_, "")] -> True
        _         -> False

processCSVFile :: String -> IO ()
processCSVFile filename = do
    content <- readFile filename
    let parsed = parseCSV content
    case parsed of
        [] -> putStrLn "Empty file"
        header:dataRows -> do
            putStrLn $ "Processing " ++ show (length dataRows) ++ " rows"
            let numericCols = numericColumns parsed
            mapM_ (\col -> 
                case columnAverage dataRows col of
                    Just avg -> putStrLn $ "Column " ++ show col ++ 
                                          " (" ++ header !! col ++ 
                                          "): average = " ++ show avg
                    Nothing  -> putStrLn $ "Column " ++ show col ++ 
                                          " has no numeric data"
                ) numericCols