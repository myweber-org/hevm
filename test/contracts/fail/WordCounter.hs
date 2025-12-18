module WordCounter where

import Data.Char (isSpace)
import System.IO (hFlush, stdout)

countWords :: String -> Int
countWords = length . words

main :: IO ()
main = do
    putStr "Enter text: "
    hFlush stdout
    input <- getLine
    let wordCount = countWords input
    putStrLn $ "Word count: " ++ show wordCount