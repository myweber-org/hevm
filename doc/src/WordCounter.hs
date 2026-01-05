
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
    isPunctuation c = c `elem` ".,!?;:\"\'()[]{}"

topWords :: Int -> WordCount -> [(String, Int)]
topWords n = take n . sortOn (Down . snd) . Map.toList

wordStats :: String -> (Int, Int, [(String, Int)])
wordStats text =
  let wc = countWords text
      totalWords = sum (Map.elems wc)
      uniqueWords = Map.size wc
      topTen = topWords 10 wc
  in (totalWords, uniqueWords, topTen)

displayStats :: String -> IO ()
displayStats text = do
  let (total, unique, top) = wordStats text
  putStrLn $ "Total words: " ++ show total
  putStrLn $ "Unique words: " ++ show unique
  putStrLn "Top 10 words:"
  mapM_ (\(w, c) -> putStrLn $ "  " ++ w ++ ": " ++ show c) top

processFile :: FilePath -> IO ()
processFile path = do
  content <- readFile path
  displayStats content