module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 10) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = 
    if all (> 0) xs 
    then Just xs 
    else Nothing

main :: IO ()
main = do
    let numbers = [5, 12, 8, 20, 3, 15]
    case validateInput numbers of
        Just validNumbers -> do
            let result = sumProcessed validNumbers
            putStrLn $ "Sum of processed numbers: " ++ show result
        Nothing -> 
            putStrLn "Invalid input: all numbers must be positive"