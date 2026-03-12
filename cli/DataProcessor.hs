
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 10) (* 2)

validateInput :: [Int] -> Maybe [Int]
validateInput [] = Nothing
validateInput xs = Just xs

main :: IO ()
main = do
    let sampleData = [5, 12, 8, 20, 3, 15]
    case validateInput sampleData of
        Nothing -> putStrLn "Empty input list"
        Just data' -> do
            let result = processData data'
            putStrLn $ "Original: " ++ show sampleData
            putStrLn $ "Processed: " ++ show result
module DataProcessor where

import Data.List (intercalate)
import Text.Read (readMaybe)

type Row = [String]
type CSVData = [Row]

filterByColumnThreshold :: CSVData -> Int -> Double -> CSVData
filterByColumnThreshold [] _ _ = []
filterByColumnThreshold (header:rows) colIndex threshold =
    header : filter (rowFilter colIndex threshold) rows
  where
    rowFilter idx thresh row
        | idx < 0 || idx >= length row = False
        | otherwise = case readMaybe (row !! idx) of
            Just val -> val > thresh
            Nothing  -> False

csvToString :: CSVData -> String
csvToString = intercalate "\n" . map (intercalate ",")

sampleData :: CSVData
sampleData =
    [ ["Name", "Age", "Score"]
    , ["Alice", "25", "85.5"]
    , ["Bob", "30", "72.0"]
    , ["Charlie", "22", "91.2"]
    , ["Diana", "28", "68.8"]
    ]

main :: IO ()
main = do
    putStrLn "Original CSV:"
    putStrLn $ csvToString sampleData
    putStrLn "\nFiltered (Score > 80.0):"
    let filtered = filterByColumnThreshold sampleData 2 80.0
    putStrLn $ csvToString filtered