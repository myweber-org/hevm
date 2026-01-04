module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumPositiveDoubled :: [Int] -> Int
sumPositiveDoubled = sum . processNumbers
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessed :: (Int -> Int) -> [Int] -> Int
sumProcessed processor = sum . map processor

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Even squares: " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of even squares: " ++ show (sumProcessed (\x -> if even x then x*x else 0) numbers)module DataProcessor where

import Data.List.Split (splitOn)

type Record = (String, Double, Double)

parseCSV :: String -> [Record]
parseCSV content = map parseLine (lines content)
  where
    parseLine line = 
        let [name, val1, val2] = splitOn "," line
        in (name, read val1, read val2)

calculateAverages :: [Record] -> (Double, Double)
calculateAverages records = 
    let (sum1, sum2, count) = foldl' agg (0, 0, 0) records
        agg (s1, s2, c) (_, v1, v2) = (s1 + v1, s2 + v2, c + 1)
    in (sum1 / fromIntegral count, sum2 / fromIntegral count)

processData :: String -> (Double, Double)
processData = calculateAverages . parseCSV

filterByThreshold :: Double -> [Record] -> [Record]
filterByThreshold threshold = filter (\(_, v1, v2) -> v1 > threshold && v2 > threshold)