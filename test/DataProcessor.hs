
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = 
    filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show (processNumbers numbers)
    putStrLn $ "Sum of processed: " ++ show (sumProcessed numbers)
    putStrLn $ "First element: " ++ show (safeHead numbers)
module DataProcessor where

import Data.List (foldl')
import Text.Read (readMaybe)

type Record = (String, Double, Double)

parseRecord :: String -> Maybe Record
parseRecord line = case words line of
    [name, val1, val2] -> do
        v1 <- readMaybe val1
        v2 <- readMaybe val2
        return (name, v1, v2)
    _ -> Nothing

parseCSV :: String -> [Record]
parseCSV content = mapMaybe parseRecord (lines content)
    where mapMaybe f = foldr (\x acc -> case f x of
                                        Just y -> y : acc
                                        Nothing -> acc) []

calculateAverages :: [Record] -> (Double, Double)
calculateAverages records = 
    if null records 
    then (0, 0)
    else (avg1, avg2)
    where
        (sum1, sum2, count) = foldl' 
            (\(s1, s2, c) (_, v1, v2) -> (s1 + v1, s2 + v2, c + 1))
            (0, 0, 0) records
        avg1 = sum1 / fromIntegral count
        avg2 = sum2 / fromIntegral count

processData :: String -> Maybe (Double, Double)
processData content = 
    let records = parseCSV content
    in if null records 
       then Nothing
       else Just (calculateAverages records)

main :: IO ()
main = do
    let sampleData = "Alice 85 90\nBob 78 82\nCharlie 92 88"
    case processData sampleData of
        Just (avg1, avg2) -> 
            putStrLn $ "Average 1: " ++ show avg1 ++ ", Average 2: " ++ show avg2
        Nothing -> 
            putStrLn "No valid data found"