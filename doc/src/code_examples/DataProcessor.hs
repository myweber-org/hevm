
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

data ValidationError
    = InvalidUsername String
    | InvalidEmail String
    | InvalidAge String
    deriving (Show, Eq)

validateUsername :: Username -> Either ValidationError Username
validateUsername name
    | null name = Left $ InvalidUsername "Username cannot be empty"
    | length name < 3 = Left $ InvalidUsername "Username must be at least 3 characters"
    | length name > 20 = Left $ InvalidUsername "Username cannot exceed 20 characters"
    | not (all isValidUsernameChar name) = Left $ InvalidUsername "Username contains invalid characters"
    | otherwise = Right (normalizeUsername name)
  where
    isValidUsernameChar c = isAlpha c || c == '_' || c == '-'
    normalizeUsername = map toLower

validateEmail :: Email -> Either ValidationError Email
validateEmail emailStr
    | '@' `notElem` emailStr = Left $ InvalidEmail "Email must contain @ symbol"
    | '.' `notElem` (dropWhile (/= '@') emailStr) = Left $ InvalidEmail "Email must contain domain with dot"
    | any isSpace emailStr = Left $ InvalidEmail "Email cannot contain spaces"
    | otherwise = Right (map toLower emailStr)

validateAge :: Age -> Either ValidationError Age
validateAge a
    | a < 0 = Left $ InvalidAge "Age cannot be negative"
    | a > 150 = Left $ InvalidAge "Age must be realistic (<= 150)"
    | otherwise = Right a

createUserProfile :: Username -> Email -> Age -> Either [ValidationError] UserProfile
createUserProfile name emailStr ageVal = case results of
    [] -> Right $ UserProfile validName validEmail validAge
    errs -> Left errs
  where
    results = catMaybes
        [ either (Just . (:[])) (const Nothing) $ validateUsername name
        , either (Just . (:[])) (const Nothing) $ validateEmail emailStr
        , either (Just . (:[])) (const Nothing) $ validateAge ageVal
        ]
    Right validName = validateUsername name
    Right validEmail = validateEmail emailStr
    Right validAge = validateAge ageVal

formatProfile :: UserProfile -> String
formatProfile (UserProfile name email age) =
    intercalate "\n"
        [ "User Profile Summary:"
        , "  Username: " ++ name
        , "  Email: " ++ email
        , "  Age: " ++ show age
        ]

processUserInput :: String -> String -> String -> String
processUserInput name emailStr ageStr =
    case (reads ageStr :: [(Int, String)]) of
        [(ageVal, "")] -> case createUserProfile name emailStr ageVal of
            Right profile -> formatProfile profile
            Left errs -> "Validation errors:\n" ++ unlines (map show errs)
        _ -> "Invalid age format. Please provide a number."

sampleUsage :: IO ()
sampleUsage = do
    putStrLn $ processUserInput "john_doe" "john@example.com" "30"
    putStrLn $ processUserInput "ab" "invalid" "-5"