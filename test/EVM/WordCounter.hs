
module WordCounter (countWords, countCharacters, countLines) where

import Data.Char (isSpace)
import Data.List (words)

countWords :: String -> Int
countWords = length . words

countCharacters :: String -> Int
countCharacters = length

countLines :: String -> Int
countLines = length . lines

processText :: String -> (Int, Int, Int)
processText text = (countWords text, countCharacters text, countLines text)

main :: IO ()
main = do
    putStrLn "Enter text (press Ctrl+D to end input):"
    input <- getContents
    let (wordCount, charCount, lineCount) = processText input
    putStrLn $ "Words: " ++ show wordCount
    putStrLn $ "Characters: " ++ show charCount
    putStrLn $ "Lines: " ++ show lineCount