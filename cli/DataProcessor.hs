
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 10) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

main :: IO ()
main = do
    let numbers = [5, 12, 8, 20, 3, 15]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processNumbers
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transform = map transform . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (* 2)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    print result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenNumbers :: [Int] -> [Int]
processEvenNumbers = filterAndTransform even (*2)

processOddNumbers :: [Int] -> [Int]
processOddNumbers = filterAndTransform odd (+1)

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original: " ++ show numbers
    putStrLn $ "Even doubled: " ++ show (processEvenNumbers numbers)
    putStrLn $ "Odd incremented: " ++ show (processOddNumbers numbers)module DataProcessor where

import Data.List (sort, group)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.Vector (Vector, toList)

type Record = (String, Int, Double)

parseCSV :: FilePath -> IO (Either String (Vector Record))
parseCSV filePath = do
    csvData <- BL.readFile filePath
    return $ Csv.decode Csv.NoHeader csvData

calculateMean :: [Double] -> Double
calculateMean xs = sum xs / fromIntegral (length xs)

calculateMedian :: [Double] -> Double
calculateMedian xs
    | null xs = 0.0
    | odd len = sorted !! mid
    | otherwise = (sorted !! (mid - 1) + sorted !! mid) / 2.0
  where
    sorted = sort xs
    len = length xs
    mid = len `div` 2

calculateMode :: [Int] -> [Int]
calculateMode xs
    | null xs = []
    | otherwise = map fst maxGroups
  where
    grouped = group $ sort xs
    maxCount = maximum $ map length grouped
    maxGroups = filter (\g -> length g == maxCount) grouped

processData :: FilePath -> IO ()
processData filePath = do
    result <- parseCSV filePath
    case result of
        Left err -> putStrLn $ "Error parsing CSV: " ++ err
        Right records -> do
            let values = map (\(_, _, val) -> val) $ toList records
                categories = map (\(_, cat, _) -> cat) $ toList records
            putStrLn $ "Mean: " ++ show (calculateMean values)
            putStrLn $ "Median: " ++ show (calculateMedian values)
            putStrLn $ "Mode of categories: " ++ show (calculateMode categories)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transform = map transform . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (* 2)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show resultmodule DataProcessor where

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

processData :: [Int] -> [Int]
processData = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let input = [1..10]
    let result = processData input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenNumbers :: [Int] -> [Int]
processEvenNumbers = filterAndTransform even (*2)

processOddNumbers :: [Int] -> [Int]
processOddNumbers = filterAndTransform odd (+1)

sumProcessedData :: (Int -> Bool) -> (Int -> Int) -> [Int] -> Int
sumProcessedData predicate transformer = 
    sum . filterAndTransform predicate transformer

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn "Original list:"
    print numbers
    putStrLn "Even numbers doubled:"
    print $ processEvenNumbers numbers
    putStrLn "Odd numbers incremented:"
    print $ processOddNumbers numbers
    putStrLn "Sum of processed odd numbers:"
    print $ sumProcessedData odd (+1) numbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (* 2)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Maybe [Int]
validateInput [] = Nothing
validateInput xs = Just xs

main :: IO ()
main = do
    let sampleData = [-3, 1, 0, 5, -2, 8]
    case validateInput sampleData of
        Nothing -> putStrLn "Empty input list"
        Just data' -> do
            let result = processData data'
            putStrLn $ "Original: " ++ show sampleData
            putStrLn $ "Processed: " ++ show resultmodule DataProcessor where

import Data.List (sort, nub)

-- Filter out even numbers from a list
filterOdds :: [Int] -> [Int]
filterOdds = filter odd

-- Square each element in a list
squareAll :: [Int] -> [Int]
squareAll = map (^2)

-- Remove duplicates and sort a list
uniqueSorted :: [Int] -> [Int]
uniqueSorted = sort . nub

-- Calculate sum of squares of odd numbers
sumOddSquares :: [Int] -> Int
sumOddSquares = sum . squareAll . filterOdds

-- Process data through a pipeline
processData :: [Int] -> [Int]
processData = uniqueSorted . squareAll . filterOdds

-- Example utility function
isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | otherwise = null [x | x <- [2..floor (sqrt (fromIntegral n))], n `mod` x == 0]module DataProcessor where

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

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Even squares: " ++ show (processEvenSquares numbers)
    putStrLn $ "First element: " ++ show (safeHead numbers)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles xs = sum $ processData xs
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

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedList :: (Int -> Int) -> [Int] -> Int
sumProcessedList processor = sum . map processor

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    
    let evenSquares = processEvenSquares numbers
    putStrLn $ "Squares of even numbers: " ++ show evenSquares
    
    let total = sumProcessedList (\x -> x * 2) numbers
    putStrLn $ "Sum of doubled numbers: " ++ show totalmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData = all (> 0) . processData

main :: IO ()
main = do
    let sampleData = [-3, 2, 0, 7, -1, 4]
    putStrLn $ "Original data: " ++ show sampleData
    putStrLn $ "Processed data: " ++ show (processData sampleData)
    putStrLn $ "Data validation: " ++ show (validateData sampleData)module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage _ [] = []
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | n > length xs = []
    | otherwise = map avg $ windows n xs
    where
        windows m ys = take (length ys - m + 1) $ iterate (drop 1) ys
        avg zs = sum zs / fromIntegral (length zs)

smoothData :: Fractional a => Int -> [a] -> [a]
smoothData window dataList = movingAverage window dataListmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbersmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-5, 3, 0, 8, -2, 10]
    let result = processNumbers numbers
    print result