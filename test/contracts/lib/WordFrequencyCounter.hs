module WordFrequencyCounter where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topNWords :: Int -> WordCount -> [(String, Int)]
topNWords n = take n . sortOn (Down . snd) . Map.toList

printWordFrequencies :: [(String, Int)] -> IO ()
printWordFrequencies = mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c)

processText :: String -> IO ()
processText text = do
  let counts = countWords text
      topWords = topNWords 10 counts
  putStrLn "Top 10 most frequent words:"
  printWordFrequencies topWords
  putStrLn $ "\nTotal unique words: " ++ show (Map.size counts)

main :: IO ()
main = do
  putStrLn "Enter text (press Ctrl+D on empty line to finish):"
  input <- getContents
  processText input