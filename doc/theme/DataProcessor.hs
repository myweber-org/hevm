
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedList :: (Int -> Int) -> [Int] -> Int
sumProcessedList processor = sum . map processor

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processEvenSquares numbers
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Even numbers squared: " ++ show result
    putStrLn $ "Sum of processed list: " ++ show (sumProcessedList (\x -> x * 2) result)