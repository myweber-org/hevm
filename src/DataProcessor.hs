module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let numbers = [-3, 1, 0, 5, -2, 8]
    let result = processNumbers numbers
    print result
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
    let sampleData = [1..10]
    putStrLn "Original data:"
    print sampleData
    
    putStrLn "\nDoubled even numbers:"
    print $ processEvenNumbers sampleData
    
    putStrLn "\nIncremented odd numbers:"
    print $ processOddNumbers sampleData
    
    putStrLn "\nSum of processed even numbers:"
    print $ sumProcessedData even (*2) sampleData
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

sumPositiveDoubled :: [Int] -> Int
sumPositiveDoubled = sum . processNumbers