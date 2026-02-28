
module DataProcessor where

processData :: [Int] -> [Int]
processData xs = map (^2) (filter even xs)