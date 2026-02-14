module WordCounter where

import Data.Char (isSpace)
import Data.List (groupBy)

countWords :: String -> Int
countWords = length . filter (not . all isSpace) . groupBy (\a b -> not (isSpace a && isSpace b))

testCountWords :: Bool
testCountWords = and
    [ countWords "" == 0
    , countWords "hello" == 1
    , countWords "hello world" == 2
    , countWords "  hello   world  " == 2
    , countWords "multiple   spaces   between" == 3
    , countWords "line1\nline2\nline3" == 3
    ]module WordCounter where

import Data.Char (isSpace)

-- | Counts the number of words in a given string.
-- Words are defined as sequences of characters separated by whitespace.
countWords :: String -> Int
countWords = length . words

-- | A more explicit version of countWords for demonstration.
countWordsExplicit :: String -> Int
countWordsExplicit str = go str False 0
  where
    go :: String -> Bool -> Int -> Int
    go [] inWord count = if inWord then count + 1 else count
    go (c:cs) inWord count
        | isSpace c = go cs False count
        | not inWord = go cs True (count + 1)
        | otherwise = go cs inWord countmodule WordCounter where

import Data.Char (isSpace)
import Data.List (words)

countWords :: String -> Int
countWords = length . words

countWordsCustom :: String -> Int
countWordsCustom = length . filter (not . null) . splitWords
  where
    splitWords = foldr splitter [[]]
    splitter c acc@(current:rest)
        | isSpace c = []:acc
        | otherwise = (c:current):rest

main :: IO ()
main = do
    let text = "Hello world! This is a test."
    putStrLn $ "Text: " ++ text
    putStrLn $ "Standard word count: " ++ show (countWords text)
    putStrLn $ "Custom word count: " ++ show (countWordsCustom text)