module DataProcessor where

processNumbers :: [Int] -> [Int]
processNumbers = map (*2) . filter (>0)