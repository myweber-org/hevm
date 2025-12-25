
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenNumbers :: [Int] -> [Int]
processEvenNumbers = filterAndTransform even (* 2)

processOddNumbers :: [Int] -> [Int]
processOddNumbers = filterAndTransform odd (+ 1)

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original: " ++ show numbers
    putStrLn $ "Even doubled: " ++ show (processEvenNumbers numbers)
    putStrLn $ "Odd incremented: " ++ show (processOddNumbers numbers)