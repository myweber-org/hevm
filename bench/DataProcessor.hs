
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

validateInput :: [Int] -> Maybe [Int]
validateInput xs
  | null xs = Nothing
  | otherwise = Just xs

main :: IO ()
main = do
  let sampleData = [-3, 0, 5, 2, -1, 8]
  case validateInput sampleData of
    Nothing -> putStrLn "Empty input list"
    Just data' -> do
      putStrLn $ "Original: " ++ show data'
      putStrLn $ "Processed: " ++ show (processData data')
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transform = map transform . filter predicate

processEvenSquares :: [Int] -> [Int]
processEvenSquares = filterAndTransform even (\x -> x * x)

sumProcessedList :: [Int] -> Int
sumProcessedList = sum . processEvenSquares
module DataProcessor where

filterAndTransform :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filterAndTransform predicate transformer = map transformer . filter predicate

processData :: [Int] -> [Int]
processData = filterAndTransform (> 0) (* 2)

sumPositiveDoubles :: [Int] -> Int
sumPositiveDoubles = sum . processDatamodule DataProcessor where

import Data.Char (toLower, isAlpha, isSpace)
import Data.List (intercalate)

data UserProfile = UserProfile
  { firstName :: String
  , lastName  :: String
  , email     :: String
  , age       :: Int
  } deriving (Show, Eq)

validateName :: String -> Maybe String
validateName name
  | all (\c -> isAlpha c || isSpace c) name && not (null name) = Just (capitalizeWords name)
  | otherwise = Nothing

capitalizeWords :: String -> String
capitalizeWords = unwords . map capitalizeWord . words
  where
    capitalizeWord [] = []
    capitalizeWord (x:xs) = toUpper x : map toLower xs
    toUpper = toEnum . subtract 32 . fromEnum

validateEmail :: String -> Bool
validateEmail email =
  let parts = splitOn '@' email
  in length parts == 2 &&
     not (null (head parts)) &&
     '.' `elem` (last parts)

splitOn :: Char -> String -> [String]
splitOn delimiter = foldr f [[]]
  where
    f c acc@(x:xs)
      | c == delimiter = []:acc
      | otherwise = (c:x):xs

validateAge :: Int -> Bool
validateAge age = age >= 0 && age <= 150

createUserProfile :: String -> String -> String -> Int -> Maybe UserProfile
createUserProfile fName lName mail userAge = do
  validFirstName <- validateName fName
  validLastName <- validateName lName
  guard (validateEmail mail)
  guard (validateAge userAge)
  return $ UserProfile validFirstName validLastName mail userAge

formatProfile :: UserProfile -> String
formatProfile profile =
  intercalate "\n"
    [ "Name: " ++ firstName profile ++ " " ++ lastName profile
    , "Email: " ++ email profile
    , "Age: " ++ show (age profile)
    ]

sanitizeInput :: String -> String
sanitizeInput = unwords . words

processUserData :: [String] -> [String] -> [Int] -> [Maybe UserProfile]
processUserData firstNames lastNames ages =
  zipWith3 createUserProfile
    (map sanitizeInput firstNames)
    (map sanitizeInput lastNames)
    (map (map toLower) firstNames ++ map (map toLower) lastNames)
    ages