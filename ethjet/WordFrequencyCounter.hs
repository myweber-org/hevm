module WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type WordCount = Map.Map T.Text Int

countWords :: T.Text -> WordCount
countWords = Map.fromListWith (+) . map (\w -> (w, 1)) . filter (not . T.null) . map normalize . T.words
  where
    normalize = T.toLower . T.filter Char.isLetter

sortByFrequency :: WordCount -> [(T.Text, Int)]
sortByFrequency = List.sortBy (\(_, c1) (_, c2) -> compare c2 c1) . Map.toList

formatOutput :: [(T.Text, Int)] -> T.Text
formatOutput = T.unlines . map (\(word, count) -> T.pack (show count) <> " " <> word)

processText :: T.Text -> T.Text
processText = formatOutput . sortByFrequency . countWords

main :: IO ()
main = do
    input <- TIO.getContents
    TIO.putStr $ processText inputmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

-- | Count frequency of words in a text string
--   Converts to lowercase and filters non-alphabetic characters
countWordFrequencies :: String -> [WordCount]
countWordFrequencies text =
  let wordsList = words text
      cleanedWords = map (filter isAlpha . map toLower) wordsList
      nonEmptyWords = filter (not . null) cleanedWords
      frequencyMap = foldr countWord [] nonEmptyWords
  in sortOn (Down . snd) frequencyMap
  where
    countWord :: String -> [WordCount] -> [WordCount]
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
      | w == word = (w, c + 1) : rest
      | otherwise = (w, c) : countWord word rest

-- | Get top N most frequent words
topNWords :: Int -> [WordCount] -> [WordCount]
topNWords n = take n

-- | Format word frequencies for display
formatFrequencies :: [WordCount] -> String
formatFrequencies freqs =
  unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqs

-- | Process a text and return formatted top N words
analyzeText :: Int -> String -> String
analyzeText n text =
  let freqs = countWordFrequencies text
      topWords = topNWords n freqs
  in formatFrequencies topWordsmodule WordFrequencyCounter where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))
import System.Environment (getArgs)

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        grouped = foldr countHelper [] wordsList
    in sortOn (Down . snd) grouped
  where
    cleanWord = map toLower . filter isAlphaNum
    countHelper word [] = [(word, 1)]
    countHelper word ((w,c):rest)
        | w == word = (w, c+1) : rest
        | otherwise = (w,c) : countHelper word rest

formatOutput :: [WordCount] -> String
formatOutput counts = unlines $ map formatLine counts
  where
    formatLine (word, count) = word ++ ": " ++ show count

processFile :: FilePath -> IO ()
processFile filename = do
    content <- readFile filename
    let counts = countWords content
    putStrLn $ formatOutput counts

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> processFile filename
        _ -> putStrLn "Usage: wordfreq <filename>"
module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordFreq = (String, Int)

countWords :: String -> [WordFreq]
countWords text = 
    let wordsList = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        grouped = group $ sort wordsList
        frequencies = map (\ws -> (head ws, length ws)) grouped
    in sortOn (Down . snd) frequencies

filterByFrequency :: Int -> [WordFreq] -> [WordFreq]
filterByFrequency minFreq = filter (\(_, count) -> count >= minFreq)

getTopNWords :: Int -> [WordFreq] -> [WordFreq]
getTopNWords n = take n

printWordFrequencies :: [WordFreq] -> IO ()
printWordFrequencies freqs = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) freqs

analyzeText :: String -> Int -> Int -> IO ()
analyzeText text minFreq topN = do
    let allFreqs = countWords text
    let filtered = filterByFrequency minFreq allFreqs
    let topWords = getTopNWords topN filtered
    
    putStrLn "Word Frequency Analysis:"
    printWordFrequencies topWords
    putStrLn $ "\nTotal unique words: " ++ show (length allFreqs)
    putStrLn $ "Words with frequency >= " ++ show minFreq ++ ": " ++ show (length filtered)