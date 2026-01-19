module DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter (>0)

validateData :: [Int] -> Bool
validateData xs = all (>=0) (processData xs)

sampleData :: [Int]
sampleData = [1, -2, 3, -4, 5]