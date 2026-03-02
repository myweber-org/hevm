
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 10) (* 2)

validateData :: [Int] -> Bool
validateData xs = all (\x -> x `mod` 2 == 0) (processData xs)

main :: IO ()
main = do
    let sampleData = [5, 12, 8, 20, 3, 15]
    putStrLn "Original data:"
    print sampleData
    putStrLn "Processed data:"
    print (processData sampleData)
    putStrLn "Validation result:"
    print (validateData sampleData)