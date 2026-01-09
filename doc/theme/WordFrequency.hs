module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))
import qualified Data.Set as Set

type Word = String
type Frequency = Int

-- Common English stopwords
stopwords :: Set.Set String
stopwords = Set.fromList ["the", "a", "an", "and", "or", "but", "in", "on", "at", "to", "for", "of", "with", "by"]

-- Simple stemming: remove common suffixes
stem :: Word -> Word
stem w
  | length w > 3 && "ing" `isSuffixOf` w = take (length w - 3) w
  | length w > 2 && "ed" `isSuffixOf` w = take (length w - 2) w
  | length w > 1 && "s" `isSuffixOf` w = take (length w - 1) w
  | otherwise = w
  where isSuffixOf suf str = suf == drop (length str - length suf) str

-- Clean and normalize a word
cleanWord :: String -> Maybe Word
cleanWord str = 
  let cleaned = map toLower $ filter isAlpha str
  in if null cleaned || Set.member cleaned stopwords 
     then Nothing 
     else Just (stem cleaned)

-- Count word frequencies in a text
countFrequencies :: String -> [(Word, Frequency)]
countFrequencies text =
  let wordsList = words text
      cleanedWords = map cleanWord wordsList
      validWords = [w | Just w <- cleanedWords]
      sortedWords = sort validWords
      grouped = group sortedWords
  in sortOn (Down . snd) [(head g, length g) | g <- grouped]

-- Pretty print frequencies
printFrequencies :: [(Word, Frequency)] -> IO ()
printFrequencies freqs = do
  putStrLn "Word frequencies:"
  mapM_ (\(w, f) -> putStrLn $ w ++ ": " ++ show f) freqs

-- Example usage
main :: IO ()
main = do
  let sampleText = "The quick brown fox jumps over the lazy dog. The dog barked at the fox."
  let freqs = countFrequencies sampleText
  printFrequencies freqs