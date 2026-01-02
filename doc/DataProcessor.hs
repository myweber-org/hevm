
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Maybe [Int]
validateInput xs
  | null xs = Nothing
  | any (< -1000) xs = Nothing
  | any (> 1000) xs = Nothing
  | otherwise = Just xs

safeProcess :: [Int] -> Maybe [Int]
safeProcess xs = do
  validated <- validateInput xs
  return $ processData validated