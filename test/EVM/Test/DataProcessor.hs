module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers
module DataProcessor where

processNumbers :: [Int] -> [Int]
processNumbers = map (^2) . filter (>0)

sumOfProcessed :: [Int] -> Int
sumOfProcessed = sum . processNumbers

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    putStrLn $ "Original list: " ++ show input
    putStrLn $ "Processed list: " ++ show (processNumbers input)
    putStrLn $ "Sum of processed: " ++ show (sumOfProcessed input)