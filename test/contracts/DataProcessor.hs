
module DataProcessor where

import Data.List (foldl')
import Text.CSV (parseCSV, Record)

type SummaryStats = (Double, Double, Double, Double)

computeStats :: [Double] -> SummaryStats
computeStats [] = (0, 0, 0, 0)
computeStats xs = (minimum xs, maximum xs, mean, stdDev)
  where
    n = fromIntegral (length xs)
    mean = sum xs / n
    variance = sum (map (\x -> (x - mean) ** 2) xs) / n
    stdDev = sqrt variance

parseNumericColumn :: String -> Maybe [Double]
parseNumericColumn input = case parseCSV "" input of
    Left _ -> Nothing
    Right csv -> extractColumn csv
  where
    extractColumn :: [Record] -> Maybe [Double]
    extractColumn [] = Nothing
    extractColumn (header:rows) = 
        if null rows then Nothing
        else traverse parseRow rows
    
    parseRow :: Record -> Maybe Double
    parseRow [] = Nothing
    parseRow (cell:_) = 
        case reads cell of
            [(val, "")] -> Just val
            _ -> Nothing

processCSVData :: String -> Maybe SummaryStats
processCSVData csvContent = do
    numericData <- parseNumericColumn csvContent
    return $ computeStats numericData

formatStats :: SummaryStats -> String
formatStats (minVal, maxVal, meanVal, stdVal) =
    "Statistics:\n" ++
    "  Minimum: " ++ show minVal ++ "\n" ++
    "  Maximum: " ++ show maxVal ++ "\n" ++
    "  Mean: " ++ show meanVal ++ "\n" ++
    "  Std Dev: " ++ show stdValmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x