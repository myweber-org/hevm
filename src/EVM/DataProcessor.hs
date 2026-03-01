module DataProcessor where

processNumbers :: [Int] -> [Int]
processNumbers = map (*2) . filter (>0)

validateAndProcess :: [Int] -> Maybe [Int]
validateAndProcess xs
    | null xs = Nothing
    | otherwise = Just (processNumbers xs)module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show result