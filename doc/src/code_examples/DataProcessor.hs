module DataProcessor where

import Data.List.Split (splitOn)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ",") (lines content)

numericColumns :: CSVData -> [[Double]]
numericColumns [] = []
numericColumns rows = 
    let transposed = transpose rows
    in map (mapMaybe readDouble) transposed
  where
    transpose :: [[a]] -> [[a]]
    transpose ([]:_) = []
    transpose x = map head x : transpose (map tail x)
    
    readDouble :: String -> Maybe Double
    readDouble s = case reads s of
        [(n, "")] -> Just n
        _ -> Nothing

calculateAverages :: [[Double]] -> [Double]
calculateAverages columns = 
    map average columns
  where
    average :: [Double] -> Double
    average [] = 0.0
    average xs = sum xs / fromIntegral (length xs)

processCSVFile :: String -> IO [Double]
processCSVFile filename = do
    content <- readFile filename
    let parsed = parseCSV content
    let numericData = numericColumns parsed
    return $ calculateAverages numericData

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) = case f x of
    Just y -> y : mapMaybe f xs
    Nothing -> mapMaybe f xs