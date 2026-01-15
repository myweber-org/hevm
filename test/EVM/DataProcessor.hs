
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformFunction numbers =
    map transformFunction (filter predicate numbers)

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)