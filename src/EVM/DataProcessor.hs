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
        numericRows = map (filter (not . null) . map maybeRead) transposed
    in map (map read) numericRows
  where
    transpose :: [[a]] -> [[a]]
    transpose [] = repeat []
    transpose ([]:xss) = transpose xss
    transpose ((x:xs):xss) = 
        (x : [h | (h:_) <- xss]) : transpose (xs : [t | (_:t) <- xss])
    
    maybeRead :: String -> String
    maybeRead s = case reads s :: [(Double, String)] of
        [(_, "")] -> s
        _ -> ""

calculateAverages :: CSVData -> [Double]
calculateAverages csvData = 
    let numericData = numericColumns csvData
        sums = map sum numericData
        counts = map fromIntegral (map length numericData)
    in zipWith (/) sums counts

processCSVFile :: FilePath -> IO [Double]
processCSVFile filePath = do
    content <- readFile filePath
    let parsedData = parseCSV content
    return $ calculateAverages parsedData

safeHead :: CSVData -> CSVRow
safeHead [] = []
safeHead (x:_) = x

validateCSV :: CSVData -> Bool
validateCSV [] = True
validateCSV (row:rows) = 
    let rowLength = length row
    in all (\r -> length r == rowLength) rowsmodule DataProcessor where

movingAverage :: Int -> [Double] -> [Double]
movingAverage windowSize xs
    | windowSize <= 0 = error "Window size must be positive"
    | length xs < windowSize = []
    | otherwise = map average $ windows windowSize xs
  where
    windows n = takeWhile (\w -> length w == n) . map (take n) . iterate tail
    average = (/ fromIntegral windowSize) . summodule DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
  where
    windows :: Int -> [a] -> [[a]]
    windows size = takeWhile ((== size) . length) . map (take size) . tails
    
    average :: Fractional a => [a] -> a
    average lst = sum lst / fromIntegral (length lst)

-- Helper function from Data.List
tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_:ys) = xs : tails ys