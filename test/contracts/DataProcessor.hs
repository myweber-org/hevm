module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, 0, 5, -7]
    let result = processNumbers input
    print resultmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

processOddCubes :: [Int] -> [Int]
processOddCubes = filterAndTransform odd (\x -> x * x * x)

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn "Original list:"
    print numbers
    putStrLn "\nEven numbers squared:"
    print $ processEvenSquares numbers
    putStrLn "\nOdd numbers cubed:"
    print $ processOddCubes numbers