module WordCounter where

import Data.Char (isSpace)
import Data.List (words)

countWords :: String -> Int
countWords = length . words

countCharacters :: String -> Int
countCharacters = length

countLines :: String -> Int
countLines = length . lines

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

main :: IO ()
main = do
    putStrLn "Enter some text (press Ctrl+D to end input on Unix/Linux, Ctrl+Z on Windows):"
    input <- getContents
    let cleaned = trim input
    putStrLn $ "Word count: " ++ show (countWords cleaned)
    putStrLn $ "Character count: " ++ show (countCharacters cleaned)
    putStrLn $ "Line count: " ++ show (countLines cleaned)