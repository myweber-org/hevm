module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)module DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter even

main :: IO ()
main = do
    let input = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    let result = processData input
    print result