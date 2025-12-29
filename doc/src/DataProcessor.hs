
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformFunction xs =
    map transformFunction (filter predicate xs)

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)