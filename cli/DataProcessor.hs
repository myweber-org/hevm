module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (^2)

calculateStats :: [Int] -> (Int, Int, Double)
calculateStats [] = (0, 0, 0.0)
calculateStats xs = 
    let total = sum xs
        count = length xs
        average = fromIntegral total / fromIntegral count
    in (total, count, average)

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn "Original list:"
    print numbers
    
    putStrLn "\nEven numbers squared:"
    let processed = processEvenSquares numbers
    print processed
    
    putStrLn "\nStatistics for processed list:"
    let (total, count, avg) = calculateStats processed
    putStrLn $ "Total: " ++ show total
    putStrLn $ "Count: " ++ show count
    putStrLn $ "Average: " ++ show avg