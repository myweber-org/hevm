module DataProcessor where

processNumbers :: [Int] -> [Int]
processNumbers = map (*2) . filter (>0)

validateAndProcess :: [Int] -> Maybe [Int]
validateAndProcess xs
    | null xs = Nothing
    | otherwise = Just (processNumbers xs)

main :: IO ()
main = do
    let sampleData = [1, -5, 3, 0, 8, -2]
    case validateAndProcess sampleData of
        Nothing -> putStrLn "Empty input list"
        Just result -> print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles xs = sum $ processNumbers xs