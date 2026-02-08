module DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter even

sampleData :: [Int]
sampleData = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

main :: IO ()
main = do
    let result = processData sampleData
    putStrLn $ "Processed data: " ++ show result