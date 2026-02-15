
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Bool
validateInput xs = not (null xs) && all (>= -1000) xs && all (<= 1000) xs

safeProcess :: [Int] -> Maybe [Int]
safeProcess xs
  | validateInput xs = Just (processData xs)
  | otherwise = Nothing

exampleUsage :: IO ()
exampleUsage = do
  let input = [1, -2, 3, 0, 5]
  case safeProcess input of
    Just result -> putStrLn $ "Processed result: " ++ show result
    Nothing -> putStrLn "Invalid input detected"