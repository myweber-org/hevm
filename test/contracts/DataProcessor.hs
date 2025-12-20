
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transform = map transform . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform even (* 2)

main :: IO ()
main = do
    let input = [1..10]
    let result = processData input
    print result