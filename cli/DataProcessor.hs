
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenNumbers :: [Int] -> [Int]
processEvenNumbers = filterAndTransform even (*2)

processOddNumbers :: [Int] -> [Int]
processOddNumbers = filterAndTransform odd (+1)

sumProcessedData :: (Int -> Bool) -> (Int -> Int) -> [Int] -> Int
sumProcessedData predicate transformer = 
    sum . filterAndTransform predicate transformer

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original numbers: " ++ show numbers
    putStrLn $ "Even numbers doubled: " ++ show (processEvenNumbers numbers)
    putStrLn $ "Odd numbers incremented: " ++ show (processOddNumbers numbers)
    putStrLn $ "Sum of processed even numbers: " ++ show (sumProcessedData even (*2) numbers)