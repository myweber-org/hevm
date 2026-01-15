module DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter evenmodule DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform even (* 2)

main :: IO ()
main = do
    let numbers = [1..10]
    let result = processNumbers numbers
    print resultmodule DataProcessor where

processEvenSquares :: [Int] -> [Int]
processEvenSquares = map (^2) . filter even

-- Example usage:
-- processEvenSquares [1,2,3,4,5,6] -> [4,16,36]