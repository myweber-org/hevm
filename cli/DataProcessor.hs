
module DataProcessor where

import Data.List.Split (splitOn)

type Row = [String]
type CSVData = [Row]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ",") (lines content)

numericColumns :: CSVData -> [[Double]]
numericColumns [] = []
numericColumns (header:rows) = 
    transpose $ map (mapMaybe readMaybe) (transpose rows)
  where
    transpose :: [[a]] -> [[a]]
    transpose [] = []
    transpose ([]:xss) = transpose xss
    transpose ((x:xs):xss) = 
        (x : [h | (h:_) <- xss]) : transpose (xs : [t | (_:t) <- xss])
    
    mapMaybe :: (a -> Maybe b) -> [a] -> [b]
    mapMaybe _ [] = []
    mapMaybe f (x:xs) = case f x of
        Just y -> y : mapMaybe f xs
        Nothing -> mapMaybe f xs
    
    readMaybe :: String -> Maybe Double
    readMaybe s = case reads s of
        [(n, "")] -> Just n
        _ -> Nothing

calculateAverages :: [[Double]] -> [Double]
calculateAverages columns = 
    map (\col -> sum col / fromIntegral (length col)) columns

processCSVFile :: String -> IO [Double]
processCSVFile filename = do
    content <- readFile filename
    let parsed = parseCSV content
    let numericData = numericColumns parsed
    return $ calculateAverages numericData

validateRowLengths :: CSVData -> Bool
validateRowLengths rows = all (== expectedLength) (map length rows)
  where
    expectedLength = if null rows then 0 else length (head rows)

filterValidRows :: CSVData -> CSVData
filterValidRows rows = 
    let expected = if null rows then 0 else length (head rows)
    in filter (\row -> length row == expected) rowsmodule DataProcessor where

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

main :: IO ()
main = do
    let numbers = [-5, 3, 0, 8, -2, 10]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | length xs < n = []
    | otherwise = avg : movingAverage n (tail xs)
    where
        window = take n xs
        avg = sum window / fromIntegral n

-- Example usage with a helper function
exampleUsage :: IO ()
exampleUsage = do
    let dataSeries = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
    let ma3 = movingAverage 3 dataSeries
    let ma5 = movingAverage 5 dataSeries
    putStrLn $ "Original series: " ++ show dataSeries
    putStrLn $ "3-period moving average: " ++ show ma3
    putStrLn $ "5-period moving average: " ++ show ma5