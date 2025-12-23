module WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type WordFrequency = Map.Map String Int

countWords :: String -> WordFrequency
countWords text =
    let wordsList = filter (not . null) $ map normalize $ words text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList
  where
    normalize = map Char.toLower . filter Char.isAlphaNum

topNWords :: Int -> String -> [(String, Int)]
topNWords n text =
    take n $ List.sortBy (\(_, cnt1) (_, cnt2) -> compare cnt2 cnt1) $
    Map.toList $ countWords text

displayFrequency :: [(String, Int)] -> String
displayFrequency freqList =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqList

processText :: Int -> String -> String
processText n = displayFrequency . topNWords nmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = filter (all isAlpha) wordsList
        grouped = group $ sort cleanedWords
    in sortOn (Down . snd) $ map (\ws -> (head ws, length ws)) grouped
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')

filterByMinFrequency :: Int -> [WordCount] -> [WordCount]
filterByMinFrequency minFreq = filter ((>= minFreq) . snd)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n

wordFrequencyReport :: String -> Int -> Int -> [WordCount]
wordFrequencyReport text minFreq topN =
    let counts = countWords text
        filtered = filterByMinFrequency minFreq counts
    in getTopNWords topN filtered

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is great. World says hello back."
    let report = wordFrequencyReport sampleText 1 5
    
    putStrLn "Word Frequency Report:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) reportmodule WordFrequencyCounter where

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
sortByFrequency = List.sortOn (negate . snd) . Map.toList

filterByMinFrequency :: Int -> WordCount -> WordCount
filterByMinFrequency minFreq = Map.filter (>= minFreq)

printWordFrequencies :: [(T.Text, Int)] -> IO ()
printWordFrequencies = mapM_ (\(word, count) -> TIO.putStrLn $ T.pack (show count) <> " " <> word)

processText :: T.Text -> Int -> IO ()
processText text minFreq = do
    let counts = countWords text
    let filtered = filterByMinFrequency minFreq counts
    let sorted = sortByFrequency filtered
    printWordFrequencies sorted

main :: IO ()
main = do
    putStrLn "Enter text (end with Ctrl+D on empty line):"
    content <- TIO.getContents
    putStrLn "Enter minimum frequency:"
    minFreqInput <- getLine
    let minFreq = read minFreqInput :: Int
    processText content minFreq