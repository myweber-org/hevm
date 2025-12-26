module DataProcessor where

import Data.List.Split (splitOn)

parseCSV :: String -> [[Double]]
parseCSV csv = map (map read . splitOn ",") (lines csv)

computeAverages :: [[Double]] -> [Double]
computeAverages rows
  | null rows = []
  | otherwise = map average (transpose rows)
  where
    average xs = sum xs / fromIntegral (length xs)
    transpose = foldr (zipWith (:)) (repeat [])

processCSVData :: String -> [Double]
processCSVData = computeAverages . parseCSVmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumPositiveDoubled :: [Int] -> Int
sumPositiveDoubled = sum . processNumbers

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

main :: IO ()
main = do
    let numbers = [-3, 2, 0, 5, -1, 8]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of positive doubled: " ++ show (sumPositiveDoubled numbers)
    putStrLn $ "First element: " ++ show (safeHead numbers)