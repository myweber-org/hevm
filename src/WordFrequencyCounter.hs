module WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map Char.toLower . filter Char.isAlphaNum

formatResults :: WordCount -> String
formatResults = unlines . map formatEntry . List.sortOn snd . Map.toList
  where
    formatEntry (word, count) = word ++ ": " ++ show count

processText :: String -> String
processText = formatResults . countWords

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: WordFrequencyCounter <text>"
    text -> putStrLn $ processText $ unwords textmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortBy, group, sort)
import Data.Ord (comparing)

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        grouped = group $ sort wordsList
    in map (\ws -> (head ws, length ws)) grouped

sortByFrequency :: [WordCount] -> [WordCount]
sortByFrequency = sortBy (flip $ comparing snd)

formatOutput :: [WordCount] -> String
formatOutput counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

processText :: String -> String
processText = formatOutput . sortByFrequency . countWords

main :: IO ()
main = do
    putStrLn "Enter text to analyze word frequency:"
    input <- getLine
    putStrLn "\nWord frequencies:"
    putStrLn $ processText inputmodule WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type WordFreq = Map.Map String Int

countWords :: String -> WordFreq
countWords text = 
    let wordsList = filter (not . null) $ map normalize $ words text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList
  where
    normalize = filter Char.isAlpha . map Char.toLower

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = 
    take n $ List.sortBy (\(_, cnt1) (_, cnt2) -> compare cnt2 cnt1) $
    Map.toList $ countWords text

displayFrequencies :: [(String, Int)] -> String
displayFrequencies freqs = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqs

processText :: Int -> String -> String
processText n = displayFrequencies . topNWords n

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell! Haskell is great. World says hello back."
    putStrLn "Top 5 most frequent words:"
    putStrLn $ processText 5 sampleTextmodule WordFrequencyCounter where

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
  where isSuffixOf suffix str = suffix == drop (length str - length suffix) str

-- Clean and normalize a word
cleanWord :: String -> Maybe Word
cleanWord str = 
  let cleaned = map toLower $ filter isAlpha str
  in if null cleaned || cleaned `Set.member` stopwords 
     then Nothing 
     else Just (stem cleaned)

-- Count word frequencies in a text
countWordFrequencies :: String -> [(Word, Frequency)]
countWordFrequencies text =
  let wordsList = words text
      cleanedWords = map cleanWord wordsList
      validWords = [w | Just w <- cleanedWords]
      sortedWords = sort validWords
      groupedWords = group sortedWords
  in sortOn (Down . snd) [(head group, length group) | group <- groupedWords]

-- Display top N frequent words
displayTopWords :: Int -> String -> IO ()
displayTopWords n text = do
  putStrLn $ "Top " ++ show n ++ " most frequent words:"
  putStrLn "----------------------------"
  let frequencies = take n $ countWordFrequencies text
  mapM_ (\(word, freq) -> putStrLn $ word ++ ": " ++ show freq) frequencies

-- Example usage
exampleText :: String
exampleText = "The quick brown fox jumps over the lazy dog. The dog barks at the fox, but the fox keeps running. Running quickly, the fox escapes."

main :: IO ()
main = do
  putStrLn "Word Frequency Counter"
  putStrLn "======================"
  displayTopWords 5 exampleText