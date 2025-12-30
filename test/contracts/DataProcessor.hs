
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

processPositiveCubes :: [Int] -> [Int]
processPositiveCubes = filterAndTransform (>0) (\x -> x * x * x)

main :: IO ()
main = do
    let testData = [-3, -2, -1, 0, 1, 2, 3, 4, 5]
    putStrLn "Original list:"
    print testData
    putStrLn "\nEven numbers squared:"
    print $ processEvenSquares testData
    putStrLn "\nPositive numbers cubed:"
    print $ processPositiveCubes testData