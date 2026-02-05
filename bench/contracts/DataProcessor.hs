module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processNumbers :: [Int] -> [Int]
processNumbers = filterAndTransform (> 0) (* 2)

main :: IO ()
main = do
    let input = [1, -2, 3, -4, 5]
    let result = processNumbers input
    print result
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessed :: (Int -> Int) -> [Int] -> Int
sumProcessed f = foldl' (\acc x -> acc + f x) 0

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Even squares: " ++ show (processEvenSquares numbers)
    putStrLn $ "Sum of doubled values: " ++ show (sumProcessed (*2) numbers)module DataProcessor where

movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs
    | n <= 0 = error "Window size must be positive"
    | length xs < n = []
    | otherwise = map average $ windows n xs
  where
    windows :: Int -> [a] -> [[a]]
    windows m = takeWhile ((== m) . length) . map (take m) . iterate tail
    
    average :: [Double] -> Double
    average ys = sum ys / fromIntegral (length ys)

safeMovingAverage :: Int -> [Double] -> Maybe [Double]
safeMovingAverage n xs
    | n <= 0 = Nothing
    | otherwise = Just $ movingAverage n xs

testData :: [Double]
testData = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]module DataProcessor where

import Data.Char (toLower, isAlpha, isSpace)
import Data.List (intercalate)

type Username = String
type Email = String
type UserProfile = (Username, Email, Int)

validateUsername :: Username -> Maybe Username
validateUsername name
    | length name >= 3 && length name <= 20 && all isAlpha name = Just name
    | otherwise = Nothing

normalizeEmail :: Email -> Email
normalizeEmail = map toLower . filter (not . isSpace)

validateEmail :: Email -> Maybe Email
validateEmail email
    | '@' `elem` email && '.' `elem` email = Just (normalizeEmail email)
    | otherwise = Nothing

createProfile :: Username -> Email -> Int -> Maybe UserProfile
createProfile username email age = do
    validName <- validateUsername username
    validEmail <- validateEmail email
    if age >= 0 && age <= 150
        then Just (validName, validEmail, age)
        else Nothing

formatProfile :: UserProfile -> String
formatProfile (name, email, age) =
    intercalate " | " ["Username: " ++ name, "Email: " ++ email, "Age: " ++ show age]

processUserData :: [String] -> [String]
processUserData inputs = 
    mapMaybe processSingle inputs
    where
        processSingle input =
            case words input of
                [name, email, ageStr] ->
                    case reads ageStr of
                        [(age, "")] -> createProfile name email age >>= Just . formatProfile
                        _ -> Nothing
                _ -> Nothing

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) =
    case f x of
        Just y -> y : mapMaybe f xs
        Nothing -> mapMaybe f xs