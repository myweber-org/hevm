module DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter even