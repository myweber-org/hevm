
module WordCounter where

import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter (not . isPunctuation)
    toLower c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c
    isPunctuation c = c `elem` ".,!?;:\"'()[]{}"

topNWords :: Int -> WordCount -> [(String, Int)]
topNWords n = take n . sortOn (Down . snd) . Map.toList

wordStats :: String -> IO ()
wordStats text = do
  let counts = countWords text
      total = sum counts
      unique = Map.size counts
      top10 = topNWords 10 counts
  
  putStrLn $ "Total words: " ++ show total
  putStrLn $ "Unique words: " ++ show unique
  putStrLn "\nTop 10 words:"
  mapM_ (\(w, c) -> putStrLn $ "  " ++ w ++ ": " ++ show c) top10

processFile :: FilePath -> IO ()
processFile path = do
  content <- readFile path
  wordStats content

main :: IO ()
main = do
  putStrLn "Enter text (empty line to finish):"
  input <- getContents
  wordStats inputmodule WordCounter where

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