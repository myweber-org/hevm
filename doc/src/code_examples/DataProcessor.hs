
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

normalizeUsername :: Username -> Maybe Username
normalizeUsername name
  | null trimmed = Nothing
  | any (not . isValidChar) trimmed = Nothing
  | length trimmed < 3 = Nothing
  | length trimmed > 20 = Nothing
  | otherwise = Just (map toLower trimmed)
  where
    trimmed = filter (not . isSpace) name
    isValidChar c = isAlpha c || c `elem` "_-"

validateEmail :: Email -> Maybe Email
validateEmail emailStr
  | '@' `notElem` emailStr = Nothing
  | '.' `notElem` localPart = Nothing
  | any isSpace emailStr = Nothing
  | length emailStr > 254 = Nothing
  | otherwise = Just emailStr
  where
    localPart = takeWhile (/= '@') emailStr

validateAge :: Int -> Maybe Age
validateAge a
  | a < 0 = Nothing
  | a > 150 = Nothing
  | otherwise = Just a

createUserProfile :: Username -> Email -> Int -> Maybe UserProfile
createUserProfile un em ag = do
  normalizedUsername <- normalizeUsername un
  validEmail <- validateEmail em
  validAge <- validateAge ag
  return $ UserProfile normalizedUsername validEmail validAge

formatProfileReport :: UserProfile -> String
formatProfileReport profile =
  intercalate "\n"
    [ "User Profile Summary:"
    , "  Username: " ++ username profile
    , "  Email: " ++ email profile
    , "  Age: " ++ show (age profile)
    ]

processUserInputs :: [(Username, Email, Int)] -> [UserProfile]
processUserInputs inputs =
  catMaybes $ map (\(u, e, a) -> createUserProfile u e a) inputs

sampleData :: [(Username, Email, Int)]
sampleData =
  [ ("John_Doe", "john@example.com", 30)
  , ("Alice-123", "alice@test.org", 25)
  , ("Invalid User", "bad-email", 200)
  , ("Bob", "bob@domain.co.uk", 42)
  ]

main :: IO ()
main = do
  let profiles = processUserInputs sampleData
  putStrLn "Valid user profiles:"
  mapM_ (putStrLn . formatProfileReport) profiles
  putStrLn $ "Total valid profiles: " ++ show (length profiles)