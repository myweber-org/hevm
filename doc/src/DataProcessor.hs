
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

processOddCubes :: [Int] -> [Int]
processOddCubes = filterAndTransform odd (\x -> x * x * x)

validateAndProcess :: [Int] -> Maybe [Int]
validateAndProcess xs
    | null xs = Nothing
    | any (< 0) xs = Nothing
    | otherwise = Just (processEvenSquares xs ++ processOddCubes xs)