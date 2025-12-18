
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateData :: [Int] -> Bool
validateData xs = all (> 0) xs && length xs > 0

main :: IO ()
main = do
    let inputData = [1, -2, 3, -4, 5]
    let processed = processData inputData
    putStrLn $ "Input: " ++ show inputData
    putStrLn $ "Processed: " ++ show processed
    putStrLn $ "Validation: " ++ show (validateData processed)