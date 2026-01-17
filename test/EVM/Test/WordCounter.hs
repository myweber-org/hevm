module WordCounter where

import Data.Char (isSpace)
import Data.List (words)

countWords :: String -> Int
countWords = length . words

main :: IO ()
main = do
    input <- getContents
    let wordCount = countWords input
    putStrLn $ "Word count: " ++ show wordCount