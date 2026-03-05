module WordCounter where

import Data.Char (isSpace, isPunctuation)
import Data.List (group, sort)

cleanText :: String -> String
cleanText = map toLower . filter (not . isPunctuation)

countWords :: String -> [(String, Int)]
countWords text = 
    let wordsList = words $ cleanText text
        sortedWords = sort wordsList
        groupedWords = group sortedWords
    in map (\ws -> (head ws, length ws)) groupedWords

mostFrequentWords :: Int -> String -> [(String, Int)]
mostFrequentWords n text = 
    take n $ reverse $ sortOn snd (countWords text)

wordFrequency :: String -> String -> Maybe Int
wordFrequency word text = 
    lookup (map toLower word) (countWords text)

processText :: String -> IO ()
processText inputText = do
    putStrLn "Word frequencies:"
    mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) (countWords inputText)
    
    putStrLn "\nTop 5 most frequent words:"
    mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) (mostFrequentWords 5 inputText)module WordCounter where

import System.Environment (getArgs)
import Data.Char (isSpace)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ splitWords text
        grouped = foldr countWord [] wordsList
    in sortOn (Down . snd) grouped
  where
    splitWords = words . map normalize
    normalize c = if isSpace c then ' ' else c
    countWord w [] = [(w, 1)]
    countWord w ((x, n):xs)
        | w == x = (x, n+1):xs
        | otherwise = (x, n):countWord w xs

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
        _ -> putStrLn "Usage: wordcounter <filename>"module WordCounter where

import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import Data.List (sortOn)

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter (not . isSpace)
    toLower c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (negate . snd) $ Map.toList (countWords text)

wordFrequencyReport :: String -> String
wordFrequencyReport text = unlines $ map formatEntry $ topNWords 10 text
  where
    formatEntry (word, count) = word ++ ": " ++ show count

processTextFile :: FilePath -> IO ()
processTextFile path = do
  content <- readFile path
  putStrLn $ "Top 10 words in " ++ path ++ ":"
  putStrLn $ wordFrequencyReport content