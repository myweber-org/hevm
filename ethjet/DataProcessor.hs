module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let input = [1..10]
    let result = processData input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumProcessed :: [Int] -> Int
sumProcessed = sum . processNumbers

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (> -100) xs then Just xs else Nothing
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = 
    filterAndTransform (\x -> x > 0 && even x) (\x -> x * 2 + 1)

calculateStats :: [Int] -> (Int, Int, Double)
calculateStats [] = (0, 0, 0.0)
calculateStats xs = 
    let total = sum xs
        count = length xs
        average = fromIntegral total / fromIntegral count
    in (total, count, average)

main :: IO ()
main = do
    let numbers = [-3, 2, 5, 8, 0, 12, 7, 4]
    let processed = processNumbers numbers
    let (total, count, avg) = calculateStats processed
    
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Processed list: " ++ show processed
    putStrLn $ "Total: " ++ show total
    putStrLn $ "Count: " ++ show count
    putStrLn $ "Average: " ++ show avg