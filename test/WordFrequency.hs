module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))
import qualified Data.Set as Set

type WordCount = (String, Int)

-- Common English stopwords
stopwords :: Set.Set String
stopwords = Set.fromList ["the", "a", "an", "and", "or", "but", "in", "on", "at", "to", "for", "of", "with", "by"]

-- Simple stemming: remove common suffixes
stem :: String -> String
stem word
  | length word > 3 && "ing" `isSuffixOf` word = take (length word - 3) word
  | length word > 2 && "ed" `isSuffixOf` word = take (length word - 2) word
  | length word > 1 && "s" `isSuffixOf` word = take (length word - 1) word
  | otherwise = word
  where isSuffixOf suffix str = suffix == drop (length str - length suffix) str

-- Clean and normalize a word
cleanWord :: String -> String
cleanWord = map toLower . filter isAlpha

-- Count word frequencies in a text
countWords :: String -> [WordCount]
countWords text = 
  let wordsList = words text
      cleaned = map cleanWord wordsList
      filtered = filter (\w -> not (Set.member w stopwords) && not (null w)) cleaned
      stemmed = map stem filtered
      grouped = group (sort stemmed)
      counts = map (\ws -> (head ws, length ws)) grouped
  in sortOn (Down . snd) counts

-- Pretty print word frequencies
printFrequencies :: [WordCount] -> IO ()
printFrequencies counts = do
  putStrLn "Word frequencies:"
  putStrLn "-----------------"
  mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

-- Example usage
main :: IO ()
main = do
  let sampleText = "The quick brown fox jumps over the lazy dog. The dog barks at the fox."
  let frequencies = countWords sampleText
  printFrequencies frequencies