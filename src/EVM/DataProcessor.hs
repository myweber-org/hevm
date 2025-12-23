
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Maybe [Int]
validateInput xs = if all (/= 0) xs then Just xs else Nothing

main :: IO ()
main = do
    let sampleData = [1, -2, 3, 0, 5, -6]
    case validateInput sampleData of
        Just validData -> print $ processData validData
        Nothing -> putStrLn "Invalid input: contains zero"