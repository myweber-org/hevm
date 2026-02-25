module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

import Data.List (intercalate)
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
      _         -> False

columnStats :: CSVData -> Int -> (Double, Double, Double)
columnStats rows colIndex = (avg, minVal, maxVal)
  where
    numericVals = [read (row !! colIndex) | row <- rows, colIndex < length row]
    avg = sum numericVals / fromIntegral (length numericVals)
    minVal = minimum numericVals
    maxVal = maximum numericVals

generateReport :: CSVData -> String
generateReport rows = 
  "CSV Analysis Report\n" ++
  "==================\n" ++
  "Total rows: " ++ show (length rows) ++ "\n" ++
  "Total columns: " ++ show (length (head rows)) ++ "\n" ++
  "Numeric columns: " ++ show (length numericCols) ++ "\n\n" ++
  "Column Statistics:\n" ++
  intercalate "\n" (map colStatStr numericCols)
  where
    numericCols = numericColumns rows
    colStatStr col = 
      "Column " ++ show col ++ " (" ++ (head rows !! col) ++ "): " ++
      "avg=" ++ show (let (a,_,_) = columnStats (tail rows) col in a) ++ ", " ++
      "min=" ++ show (let (_,m,_) = columnStats (tail rows) col in m) ++ ", " ++
      "max=" ++ show (let (_,_,x) = columnStats (tail rows) col in x)

processCSVFile :: FilePath -> IO ()
processCSVFile path = do
  content <- readFile path
  let parsed = parseCSV content
  if null parsed
    then putStrLn "Empty CSV file"
    else putStrLn $ generateReport parsed