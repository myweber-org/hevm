module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform even (\x -> x * x + 1)

main :: IO ()
main = do
    let input = [1..10]
    let result = processData input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show resultmodule DataProcessor where

movingAverage :: (Fractional a) => Int -> [a] -> [a]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
    where
        windows :: Int -> [a] -> [[a]]
        windows m = takeWhile ((== m) . length) . map (take m) . iterate tail
        
        average :: (Fractional a) => [a] -> a
        average ys = sum ys / fromIntegral (length ys)

-- Example usage:
-- movingAverage 3 [1,2,3,4,5,6] -> [2.0,3.0,4.0,5.0]