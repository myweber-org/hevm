module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordCount = Map String Int

-- | Count word frequencies in a text string
countWords :: String -> WordCount
countWords text = Map.fromListWith (+) [(w, 1) | w <- words (normalizeText text)]

-- | Normalize text by converting to lowercase and removing non-alphabetic characters
normalizeText :: String -> String
normalizeText = unwords . map (filter isAlpha . map toLower) . words

-- | Get top N most frequent words
topNWords :: Int -> WordCount -> [(String, Int)]
topNWords n = take n . sortOn (\(_, count) -> negate count) . Map.toList

-- | Pretty print word frequencies
printWordFrequencies :: [(String, Int)] -> String
printWordFrequencies freqs = unlines [word ++ ": " ++ show count | (word, count) <- freqs]

-- | Process a text and return top N words
analyzeText :: Int -> String -> String
analyzeText n text = printWordFrequencies $ topNWords n (countWords text)module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordFreq = (String, Int)

-- | Count frequency of each word in a text string
countWordFrequencies :: String -> [WordFreq]
countWordFrequencies text =
    let wordsList = filter (not . null) $ map normalizeWord $ words text
        freqMap = foldr (\word acc -> insertWord word acc) [] wordsList
    in sortOn (Down . snd) freqMap
  where
    normalizeWord = map toLower . filter isAlpha
    
    insertWord :: String -> [WordFreq] -> [WordFreq]
    insertWord word [] = [(word, 1)]
    insertWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : insertWord word rest

-- | Get top N most frequent words
topNWords :: Int -> String -> [WordFreq]
topNWords n text = take n $ countWordFrequencies text

-- | Calculate word frequency distribution as percentages
frequencyDistribution :: String -> [(String, Double)]
frequencyDistribution text =
    let freqs = countWordFrequencies text
        total = fromIntegral $ sum $ map snd freqs
    in map (\(w, c) -> (w, (fromIntegral c / total) * 100)) freqs

-- | Filter words by minimum frequency threshold
filterByMinFrequency :: Int -> String -> [WordFreq]
filterByMinFrequency minFreq text =
    filter (\(_, count) -> count >= minFreq) $ countWordFrequencies text
module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordFreq = (String, Int)

countWords :: String -> [WordFreq]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        grouped = group $ sort cleanedWords
        frequencies = map (\ws -> (head ws, length ws)) grouped
    in sortOn (Down . snd) frequencies
  where
    cleanWord = filter isAlpha

filterByFrequency :: Int -> [WordFreq] -> [WordFreq]
filterByFrequency minCount = filter (\(_, count) -> count >= minCount)

getTopNWords :: Int -> [WordFreq] -> [WordFreq]
getTopNWords n = take n

formatOutput :: [WordFreq] -> String
formatOutput freqs = unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqs

processText :: String -> Int -> Int -> String
processText text minFreq topN = 
    let freqs = countWords text
        filtered = filterByFrequency minFreq freqs
        topWords = getTopNWords topN filtered
    in formatOutput topWords

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is great. World says hello to Haskell."
    putStrLn "Word Frequency Analysis:"
    putStrLn $ processText sampleText 1 5