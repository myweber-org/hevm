
module DataProcessor where

processNumbers :: [Int] -> [Int]
processNumbers = map (^2) . filter (>0)

validateAndProcess :: [Int] -> Maybe [Int]
validateAndProcess xs
    | null xs = Nothing
    | otherwise = Just (processNumbers xs)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    case validateAndProcess input of
        Nothing -> putStrLn "Empty list provided"
        Just result -> print result