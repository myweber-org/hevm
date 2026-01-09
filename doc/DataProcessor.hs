
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = 
    map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedData :: (Int -> Bool) -> (Int -> Int) -> [Int] -> Int
sumProcessedData predicate transformer = 
    sum . filterAndTransform predicate transformer

main :: IO ()
main = do
    let dataSet = [1..10]
    putStrLn $ "Original data: " ++ show dataSet
    putStrLn $ "Even squares: " ++ show (processEvenSquares dataSet)
    putStrLn $ "Sum of even squares: " ++ 
        show (sumProcessedData even (\x -> x * x) dataSet)