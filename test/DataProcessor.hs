
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateAndProcess :: [Int] -> Maybe [Int]
validateAndProcess xs
    | null xs = Nothing
    | otherwise = Just $ processData xs

main :: IO ()
main = do
    let inputData = [1, -2, 3, 0, 5, -8]
    case validateAndProcess inputData of
        Nothing -> putStrLn "Empty input list"
        Just result -> print resultmodule DataProcessor where

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