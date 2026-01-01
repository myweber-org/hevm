module DataProcessor where

processData :: [Int] -> [Int]
processData = map (^2) . filter (>0)

validateData :: [Int] -> Bool
validateData xs = all (>=0) xs && not (null xs)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    if validateData input
        then print $ processData input
        else putStrLn "Invalid input data"