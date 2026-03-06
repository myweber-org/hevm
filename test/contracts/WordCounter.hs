module WordCounter where

import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map

type WordFrequency = Map.Map String Int

countWords :: String -> WordFrequency
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topWords :: Int -> String -> [(String, Int)]
topWords n text = take n $ sortOn (negate . snd) $ Map.toList (countWords text)

displayResults :: [(String, Int)] -> String
displayResults = unlines . map (\(w, c) -> w ++ ": " ++ show c)

analyzeText :: Int -> String -> String
analyzeText n = displayResults . topWords n
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

topWords :: Int -> String -> [(String, Int)]
topWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

wordFrequency :: String -> [(String, Double)]
wordFrequency text =
  let counts = countWords text
      total = fromIntegral $ sum $ Map.elems counts
  in map (\(w, c) -> (w, fromIntegral c / total)) $ Map.toList counts

uniqueWords :: String -> Int
uniqueWords = Map.size . countWords

totalWords :: String -> Int
totalWords = length . words

analyzeText :: String -> IO ()
analyzeText text = do
  putStrLn $ "Total words: " ++ show (totalWords text)
  putStrLn $ "Unique words: " ++ show (uniqueWords text)
  putStrLn "\nTop 10 words:"
  mapM_ (\(w, c) -> putStrLn $ "  " ++ w ++ ": " ++ show c) $ topWords 10 text
  putStrLn "\nWord frequencies:"
  mapM_ (\(w, f) -> putStrLn $ "  " ++ w ++ ": " ++ show (round (f * 10000) / 100) ++ "%") 
        $ take 5 $ wordFrequency text
module WordCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type WordFreq = Map.Map String Int

countWords :: String -> WordFreq
countWords text =
    let wordsList = filter (not . null) $ map normalize $ splitWords text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList

splitWords :: String -> [String]
splitWords = words . map (\c -> if Char.isAlpha c then Char.toLower c else ' ')

normalize :: String -> String
normalize = filter Char.isAlpha

topNWords :: Int -> String -> [(String, Int)]
topNWords n text =
    take n $ List.sortBy (\(_, cnt1) (_, cnt2) -> compare cnt2 cnt1) $
    Map.toList $ countWords text

displayFrequencies :: [(String, Int)] -> String
displayFrequencies freqs =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqs

processText :: Int -> String -> String
processText n text = displayFrequencies $ topNWords n text