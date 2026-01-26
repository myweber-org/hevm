module DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter even

-- Example usage:
-- processData [1,2,3,4,5] -> [4,16]