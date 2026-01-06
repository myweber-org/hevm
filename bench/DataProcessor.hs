module DataProcessor where

filterAndSquareEvens :: [Int] -> [Int]
filterAndSquareEvens = map (^2) . filter even

processData :: [Int] -> [Int]
processData = filterAndSquareEvens