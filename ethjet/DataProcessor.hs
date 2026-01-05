module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (\x -> x * x + 1)

validateInput :: [Int] -> Maybe [Int]
validateInput xs
    | null xs = Nothing
    | any (< 0) xs = Nothing
    | otherwise = Just xs

main :: IO ()
main = do
    let sampleData = [1..10]
    case validateInput sampleData of
        Just validData -> do
            putStrLn "Original data:"
            print validData
            putStrLn "Processed data (even numbers squared plus one):"
            print $ processNumbers validData
        Nothing -> putStrLn "Invalid input data"module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print result