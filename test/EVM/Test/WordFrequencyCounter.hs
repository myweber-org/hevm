module WordFrequencyCounter where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordFreq = (String, Int)

countWordFrequencies :: String -> [WordFreq]
countWordFrequencies text =
    let wordsList = filter (not . null) $ map cleanWord $ words text
        freqMap = foldl (\acc word -> insertWord word acc) [] wordsList
    in sortOn (Down . snd) freqMap
  where
    cleanWord = map toLower . filter isAlphaNum
    insertWord word [] = [(word, 1)]
    insertWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : insertWord word rest

displayFrequencies :: [WordFreq] -> String
displayFrequencies freqs =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqs

processText :: String -> String
processText = displayFrequencies . countWordFrequenciesmodule WordFrequencyCounter where

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

filterByMinFrequency :: Int -> WordCount -> WordCount
filterByMinFrequency minFreq = Map.filter (>= minFreq)

processText :: T.Text -> Int -> [(T.Text, Int)]
processText text minFreq = sortByFrequency $ filterByMinFrequency minFreq $ countWords text

printResults :: [(T.Text, Int)] -> IO ()
printResults = mapM_ (\(word, count) -> TIO.putStrLn $ T.pack (show count) <> " " <> word)

main :: IO ()
main = do
    content <- TIO.readFile "input.txt"
    let results = processText content 3
    printResults resultsmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordCount = Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word acc =
      let cleaned = cleanWord word
       in if null cleaned
            then acc
            else Map.insertWith (+) cleaned 1 acc

cleanWord :: String -> String
cleanWord = map toLower . filter isAlpha

getTopWords :: Int -> WordCount -> [(String, Int)]
getTopWords n = take n . sortOn (\(_, count) -> negate count) . Map.toList

processText :: Int -> String -> [(String, Int)]
processText n = getTopWords n . countWords

prettyPrint :: [(String, Int)] -> String
prettyPrint = unlines . map (\(word, count) -> word ++ ": " ++ show count)

main :: IO ()
main = do
  let sampleText = "Hello world! Hello Haskell. Haskell is great. World says hello to Haskell."
  putStrLn "Top 3 most frequent words:"
  putStrLn $ prettyPrint $ processText 3 sampleText