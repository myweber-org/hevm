
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers
module DataProcessor where

import Data.List (foldl')
import Text.CSV

type Row = [String]
type CSVData = [Row]

data SummaryStats = SummaryStats
    { count :: Int
    , sum   :: Double
    , mean  :: Double
    , min   :: Double
    , max   :: Double
    } deriving (Show, Eq)

parseCSV :: String -> Either String CSVData
parseCSV input = case parseCSV input of
    Left err -> Left $ "Parse error: " ++ err
    Right csv -> Right csv

computeStats :: CSVData -> Int -> Either String SummaryStats
computeStats [] _ = Left "Empty dataset"
computeStats rows colIndex
    | colIndex < 0 = Left "Column index must be non-negative"
    | otherwise = case mapM parseRow rows of
        Left err -> Left err
        Right values -> Right $ calculate values
  where
    parseRow row
        | colIndex >= length row = Left "Column index out of bounds"
        | otherwise = case reads (row !! colIndex) of
            [(val, "")] -> Right val
            _ -> Left $ "Invalid number format in column " ++ show colIndex

    calculate vals = SummaryStats
        { count = length vals
        , sum   = total
        , mean  = total / fromIntegral (length vals)
        , min   = minimum vals
        , max   = maximum vals
        }
      where
        total = foldl' (+) 0 vals

printStats :: SummaryStats -> IO ()
printStats stats = do
    putStrLn $ "Count: " ++ show (count stats)
    putStrLn $ "Sum:   " ++ show (sum stats)
    putStrLn $ "Mean:  " ++ show (mean stats)
    putStrLn $ "Min:   " ++ show (min stats)
    putStrLn $ "Max:   " ++ show (max stats)

processCSVFile :: FilePath -> Int -> IO ()
processCSVFile filePath colIndex = do
    content <- readFile filePath
    case parseCSV content of
        Left err -> putStrLn $ "Error: " ++ err
        Right csvData -> case computeStats csvData colIndex of
            Left err -> putStrLn $ "Error: " ++ err
            Right stats -> printStats statsmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers