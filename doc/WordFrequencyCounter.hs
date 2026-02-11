module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map normalize $ words text
        normalized = map toLower . filter isAlpha
        normalize = normalized
        frequencies = foldr countWord [] wordsList
        countWord word [] = [(word, 1)]
        countWord word ((w, c):rest)
            | word == w = (w, c + 1) : rest
            | otherwise = (w, c) : countWord word rest
    in sortOn (Down . snd) frequencies

topNWords :: Int -> String -> [WordCount]
topNWords n = take n . countWords

testText :: String
testText = "The quick brown fox jumps over the lazy dog. The dog barks at the fox."

main :: IO ()
main = do
    putStrLn "Word frequencies:"
    mapM_ print $ countWords testText
    putStrLn "\nTop 3 words:"
    mapM_ print $ topNWords 3 testTextmodule WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified System.Environment as Env

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord w = Map.insertWith (+) (normalize w) 1
    normalize = map Char.toLower . filter Char.isAlphaNum

formatResults :: WordCount -> String
formatResults = unlines . map formatEntry . List.sortOn (negate . snd) . Map.toList
  where
    formatEntry (word, count) = word ++ ": " ++ show count

main :: IO ()
main = do
    args <- Env.getArgs
    case args of
        [] -> putStrLn "Usage: wordfreq <text>"
        textPieces -> do
            let text = unwords textPieces
            let counts = countWords text
            putStrLn $ formatResults countsmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        grouped = group $ sort cleanedWords
        counts = map (\ws -> (head ws, length ws)) grouped
    in sortOn (Down . snd) counts
  where
    cleanWord = filter isAlpha

filterByMinimum :: Int -> [WordCount] -> [WordCount]
filterByMinimum minCount = filter (\(_, count) -> count >= minCount)

getTopN :: Int -> [WordCount] -> [WordCount]
getTopN n = take n

printWordCounts :: [WordCount] -> IO ()
printWordCounts counts = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

processText :: String -> Int -> Int -> IO ()
processText text minCount topN = do
    let allCounts = countWords text
        filtered = filterByMinimum minCount allCounts
        topResults = getTopN topN filtered
    
    putStrLn $ "Total unique words: " ++ show (length allCounts)
    putStrLn $ "Words with at least " ++ show minCount ++ " occurrences: " ++ show (length filtered)
    putStrLn "Top words:"
    printWordCounts topResults

sampleText :: String
sampleText = "This is a sample text. This text contains words. Some words repeat. Text analysis is useful."

main :: IO ()
main = processText sampleText 2 5