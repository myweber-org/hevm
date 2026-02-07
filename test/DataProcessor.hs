
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenNumbers :: [Int] -> [Int]
processEvenNumbers = filterAndTransform even (*2)

sumProcessedNumbers :: [Int] -> Int
sumProcessedNumbers = sum . processEvenNumbers

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processEvenNumbers numbers)
    putStrLn $ "Sum of processed numbers: " ++ show (sumProcessedNumbers numbers)module DataProcessor where

import Data.List.Split (splitOn)

parseCSV :: String -> [[Double]]
parseCSV content = map (map read . splitOn ",") $ lines content

calculateAverages :: [[Double]] -> [Double]
calculateAverages rows
    | null rows = []
    | otherwise = map avg $ transpose rows
  where
    avg xs = sum xs / fromIntegral (length xs)
    transpose = foldr (zipWith (:)) (repeat [])

processCSVData :: String -> [Double]
processCSVData = calculateAverages . parseCSVmodule DataProcessor where

import Data.List (tails)

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | n > length xs = error "Window size exceeds list length"
    | otherwise = map average $ filter (\window -> length window == n) $ tails xs
  where
    average :: [Double] -> Double
    average ws = sum ws / fromIntegral (length ws)

-- Example usage
sampleData :: [Double]
sampleData = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]

main :: IO ()
main = do
    let result = movingAverage 3 sampleData
    putStrLn "Moving average with window size 3:"
    print resultmodule DataProcessor where

movingAverage :: (Fractional a) => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
  where
    windows :: Int -> [a] -> [[a]]
    windows m ys = take (length ys - m + 1) $ zipWith (:) ys (tails ys)
    
    average :: (Fractional a) => [a] -> a
    average zs = sum zs / fromIntegral (length zs)

-- Helper function similar to Data.List.tails
tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = (x:xs) : tails xs