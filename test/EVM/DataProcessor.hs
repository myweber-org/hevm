module DataProcessor where

import Data.List.Split (splitOn)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ",") (lines content)

numericFields :: Row -> [Double]
numericFields row = map read (filter isNumeric row)
  where
    isNumeric s = case reads s :: [(Double, String)] of
      [(_, "")] -> True
      _         -> False

calculateAverages :: CSVData -> [Double]
calculateAverages csvData =
  let numericRows = map numericFields csvData
      transposed = transpose numericRows
  in map average transposed
  where
    transpose :: [[Double]] -> [[Double]]
    transpose [] = []
    transpose ([]:_) = []
    transpose xss = map head xss : transpose (map tail xss)
    
    average :: [Double] -> Double
    average [] = 0.0
    average xs = sum xs / fromIntegral (length xs)

processCSVFile :: String -> IO [Double]
processCSVFile filename = do
  content <- readFile filename
  let parsed = parseCSV content
  return (calculateAverages parsed)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

validateInput :: [Int] -> Maybe [Int]
validateInput xs
    | null xs = Nothing
    | any (< -100) xs = Nothing
    | any (> 100) xs = Nothing
    | otherwise = Just xs

safeDataProcessing :: [Int] -> Maybe Int
safeDataProcessing xs = do
    validated <- validateInput xs
    return $ sumProcessedData validated