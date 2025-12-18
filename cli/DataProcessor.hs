
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
    print result