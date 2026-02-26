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
module DataProcessor where

import Data.Time
import Text.CSV

filterCSVByDate :: String -> Day -> Day -> Either String [Record]
filterCSVByDate csvContent startDate endDate = do
    csv <- parseCSV "input" csvContent
    let filtered = filter (isWithinDateRange startDate endDate) csv
    return filtered

isWithinDateRange :: Day -> Day -> Record -> Bool
isWithinDateRange start end record =
    case record of
        (dateStr:_) -> 
            case parseDate dateStr of
                Just date -> date >= start && date <= end
                Nothing -> False
        _ -> False

parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 10) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just xmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbersmodule DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | length xs < n = []
    | otherwise = avg : movingAverage n (tail xs)
  where
    window = take n xs
    avg = sum window / fromIntegral n

-- Example usage with a helper function
demoMovingAverage :: IO ()
demoMovingAverage = do
    let dataSeries = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
    putStrLn "Original series:"
    print dataSeries
    putStrLn "\n3-period moving average:"
    print $ movingAverage 3 dataSeries
    putStrLn "\n5-period moving average:"
    print $ movingAverage 5 dataSeries