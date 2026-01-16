
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedList :: [Int] -> Int
sumProcessedList = foldl' (+) 0 . processEvenSquares
  where
    foldl' f acc [] = acc
    foldl' f acc (x:xs) = foldl' f (f acc x) xs