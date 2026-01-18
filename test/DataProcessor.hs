
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenNumbers :: [Int] -> [Int]
processEvenNumbers = 
    filterAndTransform even (* 2)

processOddNumbers :: [Int] -> [Int]
processOddNumbers = 
    filterAndTransform odd (+ 1)

sumProcessedData :: (Int -> Bool) -> (Int -> Int) -> [Int] -> Int
sumProcessedData predicate transformer = 
    sum . filterAndTransform predicate transformermodule DataProcessor where

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