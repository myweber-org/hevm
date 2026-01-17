
module DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter (>0)

validateData :: [Int] -> Bool
validateData xs = all (>0) xs && length xs <= 100

main :: IO ()
main = do
    let sample = [1, -2, 3, -4, 5]
    putStrLn $ "Original: " ++ show sample
    putStrLn $ "Processed: " ++ show (processData sample)
    putStrLn $ "Validation: " ++ show (validateData (processData sample))