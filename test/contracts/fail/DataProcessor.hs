
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenNumbers :: [Int] -> [Int]
processEvenNumbers = 
    filterAndTransform even (\x -> x * 2 + 1)

sumProcessedEvenNumbers :: [Int] -> Int
sumProcessedEvenNumbers = 
    sum . processEvenNumbers

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed even numbers: " ++ show (processEvenNumbers numbers)
    putStrLn $ "Sum of processed even numbers: " ++ show (sumProcessedEvenNumbers numbers)
    putStrLn $ "First element (safe): " ++ show (safeHead numbers)
    putStrLn $ "First element of empty list: " ++ show (safeHead [])
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)