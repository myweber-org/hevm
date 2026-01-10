module DataProcessor where

processEvenSquares :: [Int] -> [Int]
processEvenSquares = map (^2) . filter even

exampleUsage :: IO ()
exampleUsage = do
    let input = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    let result = processEvenSquares input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result