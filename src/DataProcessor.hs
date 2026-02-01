module DataProcessor where

processEvenSquares :: [Int] -> [Int]
processEvenSquares = map (^2) . filter even