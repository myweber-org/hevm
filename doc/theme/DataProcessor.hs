module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processNumbers input
    print resultmodule DataProcessor where

import Data.List (tails)

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | n > length xs = error "Window size exceeds list length"
    | otherwise = map avg $ filter (\window -> length window == n) $ tails xs
  where
    avg :: [Double] -> Double
    avg window = sum window / fromIntegral n

smoothData :: Int -> [Double] -> [Double]
smoothData windowSize dataPoints =
    movingAverage windowSize dataPoints

validateData :: [Double] -> Maybe [Double]
validateData [] = Nothing
validateData xs
    | any isInfinite xs = Nothing
    | any isNaN xs = Nothing
    | otherwise = Just xs
  where
    isInfinite x = x == 1/0 || x == -1/0
    isNaN x = x /= x