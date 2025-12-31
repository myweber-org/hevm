
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = all (\x -> x >= -100 && x <= 100) xs

safeProcess :: [Int] -> Maybe [Int]
safeProcess xs
    | validateInput xs = Just (processData xs)
    | otherwise = Nothing
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumProcessedData :: [Int] -> Int
sumProcessedData = sum . processData

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5, 0, 7]
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Processed: " ++ show (processData input)
    putStrLn $ "Sum: " ++ show (sumProcessedData input)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessed :: (Int -> Int) -> [Int] -> Int
sumProcessed processor = sum . map processor

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn "Original list:"
    print numbers
    
    putStrLn "\nSquares of even numbers:"
    let squares = processEvenSquares numbers
    print squares
    
    putStrLn "\nSum of squares of even numbers:"
    print (sumProcessed (\x -> x * x) (filter even numbers))