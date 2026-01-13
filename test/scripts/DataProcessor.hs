
module DataProcessor where

import Data.Char (toLower, isAlpha, isSpace)
import Data.List (intercalate)
import Data.Maybe (catMaybes)

type Username = String
type Email = String
type Age = Int

data UserProfile = UserProfile
  { username :: Username
  , email :: Email
  , age :: Age
  } deriving (Show, Eq)

validateUsername :: Username -> Maybe Username
validateUsername name
  | length name < 3 = Nothing
  | length name > 20 = Nothing
  | not (all isValidChar name) = Nothing
  | otherwise = Just (normalizeUsername name)
  where
    isValidChar c = isAlpha c || c == '_' || c == '-'
    normalizeUsername = map toLower

validateEmail :: Email -> Maybe Email
validateEmail emailStr
  | '@' `notElem` emailStr = Nothing
  | '.' `notElem` (dropWhile (/= '@') emailStr) = Nothing
  | any isSpace emailStr = Nothing
  | otherwise = Just (map toLower emailStr)

validateAge :: Age -> Maybe Age
validateAge a
  | a < 0 = Nothing
  | a > 150 = Nothing
  | otherwise = Just a

createUserProfile :: Username -> Email -> Age -> Maybe UserProfile
createUserProfile name emailStr ageVal = do
  validName <- validateUsername name
  validEmail <- validateEmail emailStr
  validAge <- validateAge ageVal
  return $ UserProfile validName validEmail validAge

sanitizeInput :: String -> String
sanitizeInput = unwords . words

formatProfileSummary :: UserProfile -> String
formatProfileSummary profile =
  intercalate "\n"
    [ "User Profile Summary:"
    , "Username: " ++ username profile
    , "Email: " ++ email profile
    , "Age: " ++ show (age profile)
    ]

processUserBatch :: [(Username, Email, Age)] -> [UserProfile]
processUserBatch = catMaybes . map (\(n,e,a) -> createUserProfile n e a)

exampleUsage :: IO ()
exampleUsage = do
  let testData = 
        [ ("john_doe", "john@example.com", 25)
        , ("a", "invalid", -5)
        , ("verylongusername12345", "test@domain.com", 30)
        ]
  
  putStrLn "Processing user batch:"
  mapM_ (putStrLn . formatProfileSummary) (processUserBatch testData)