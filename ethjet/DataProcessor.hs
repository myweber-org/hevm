
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processData input
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform isEven double
  where
    isEven x = x `mod` 2 == 0
    double x = x * 2

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (>0) xs then Just xs else Nothing

main :: IO ()
main = do
    let numbers = [1,2,3,4,5,6,7,8,9,10]
    case validateInput numbers of
        Just validNumbers -> do
            putStrLn $ "Original list: " ++ show validNumbers
            putStrLn $ "Processed list: " ++ show (processNumbers validNumbers)
            putStrLn $ "Sum of processed: " ++ show (sumProcessed validNumbers)
        Nothing -> putStrLn "Invalid input: all numbers must be positive"