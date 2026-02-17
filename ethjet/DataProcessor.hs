module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = all (\x -> x >= -100 && x <= 100) xs

main :: IO ()
main = do
    let sampleData = [-5, 2, 0, 8, -3, 10]
    if validateInput sampleData
        then do
            putStrLn "Processing valid data..."
            let result = processData sampleData
            putStrLn $ "Input: " ++ show sampleData
            putStrLn $ "Result: " ++ show result
        else
            putStrLn "Invalid input detected"module DataProcessor where

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage windowSize xs
    | windowSize <= 0 = error "Window size must be positive"
    | length xs < windowSize = []
    | otherwise = map average $ windows windowSize xs
  where
    average ws = sum ws / fromIntegral (length ws)
    windows n = takeWhile (\w -> length w == n) . map (take n) . tails