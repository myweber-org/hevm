module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Maybe [Int]
validateInput xs
    | null xs = Nothing
    | otherwise = Just xs

main :: IO ()
main = do
    let numbers = [1, -2, 3, 0, 5, -8]
    case validateInput numbers of
        Nothing -> putStrLn "Empty input list"
        Just validNumbers -> do
            let result = processNumbers validNumbers
            putStrLn $ "Original: " ++ show numbers
            putStrLn $ "Processed: " ++ show result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processNumbers input
    print result