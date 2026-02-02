module DataProcessor where

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

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

processOddCubes :: [Int] -> [Int]
processOddCubes = filterAndTransform odd (\x -> x * x * x)

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn "Original list:"
    print numbers
    putStrLn "\nEven numbers squared:"
    print $ processEvenSquares numbers
    putStrLn "\nOdd numbers cubed:"
    print $ processOddCubes numbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processDatamodule DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
  where
    windows :: Int -> [a] -> [[a]]
    windows size list = take (length list - size + 1) $ 
                        map (take size) (tails list)
    
    average :: Fractional a => [a] -> a
    average lst = sum lst / fromIntegral (length lst)

-- Helper function from Data.List
tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_:ys) = xs : tails ys
module DataProcessor where

import Data.List (foldl')
import Text.Read (readMaybe)

type Row = [String]
type CSVData = [Row]

data SummaryStats = SummaryStats
    { count :: Int
    , sum   :: Double
    , mean  :: Double
    , min   :: Double
    , max   :: Double
    } deriving (Show, Eq)

parseCSV :: String -> CSVData
parseCSV = map (splitOn ',') . lines
  where
    splitOn :: Char -> String -> [String]
    splitOn delim = foldr splitHelper [""]
      where
        splitHelper ch (x:xs)
            | ch == delim = "":x:xs
            | otherwise   = (ch:x):xs

safeReadDouble :: String -> Maybe Double
safeReadDouble = readMaybe

computeColumnStats :: CSVData -> Int -> Maybe SummaryStats
computeColumnStats rows colIndex
    | null validValues = Nothing
    | otherwise = Just SummaryStats
        { count = length validValues
        , sum   = total
        , mean  = total / fromIntegral (length validValues)
        , min   = minimum validValues
        , max   = maximum validValues
        }
  where
    extractValue :: Row -> Maybe Double
    extractValue row
        | colIndex < length row = safeReadDouble (row !! colIndex)
        | otherwise = Nothing
    
    validValues = [v | Just v <- map extractValue rows]
    total = foldl' (+) 0 validValues

processCSVFile :: String -> Int -> IO (Maybe SummaryStats)
processCSVFile filePath columnIndex = do
    content <- readFile filePath
    let parsedData = parseCSV content
    return $ computeColumnStats parsedData columnIndex

validateCSV :: CSVData -> Bool
validateCSV [] = True
validateCSV (firstRow:rows) = all (\row -> length row == length firstRow) rows

filterRows :: (Row -> Bool) -> CSVData -> CSVData
filterRows predicate = filter predicate

mapColumn :: (String -> String) -> Int -> CSVData -> CSVData
mapColumn f colIndex = map (mapAtIndex colIndex f)
  where
    mapAtIndex idx func row
        | idx < length row = take idx row ++ [func (row !! idx)] ++ drop (idx + 1) row
        | otherwise = rowmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)