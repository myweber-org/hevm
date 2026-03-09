module WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords = foldr updateCount Map.empty . words
  where
    updateCount word = Map.insertWith (+) (normalize word) 1
    normalize = filter isAlpha . map toLower

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

displayFrequencies :: [(String, Int)] -> String
displayFrequencies = unlines . map (\(w, c) -> w ++ ": " ++ show c)

analyzeText :: Int -> String -> String
analyzeText n = displayFrequencies . topNWords nmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map toLower <$> wordsList
        grouped = foldr countHelper [] cleaned
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | word == w = (w, c + 1) : rest
        | otherwise = (w, c) : countHelper word rest

displayFrequency :: [WordCount] -> String
displayFrequency counts = 
    unlines $ "Top 10 most frequent words:" : map formatCount counts
  where
    formatCount (word, count) = word ++ ": " ++ show count

analyzeText :: String -> String
analyzeText = displayFrequency . countWordsmodule WordFrequency where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords text =
    let wordsList = filter (not . null) $ map normalize $ words text
    in List.foldl' (\acc w -> Map.insertWith (+) w 1 acc) Map.empty wordsList
  where
    normalize = filter Char.isAlpha . map Char.toLower

topNWords :: Int -> String -> [(String, Int)]
topNWords n text =
    take n $ List.sortBy (\(_, cnt1) (_, cnt2) -> compare cnt2 cnt1) $
    Map.toList $ countWords text

displayFrequency :: [(String, Int)] -> String
displayFrequency freqList =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqList

processText :: Int -> String -> String
processText n = displayFrequency . topNWords nmodule WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type FrequencyMap = Map.Map T.Text Int

countWords :: T.Text -> FrequencyMap
countWords = foldr (\word -> Map.insertWith (+) word 1) Map.empty . words
  where
    words = filter (not . T.null) . map (T.filter isAlpha . T.toLower) . T.split (not . isAlpha)

readFileAndCount :: FilePath -> IO FrequencyMap
readFileAndCount path = countWords <$> TIO.readFile path

printFrequencies :: FrequencyMap -> IO ()
printFrequencies freqMap = mapM_ printEntry sorted
  where
    sorted = sortOn (negate . snd) $ Map.toList freqMap
    printEntry (word, count) = TIO.putStrLn $ T.pack (show count) <> " " <> word

main :: IO ()
main = do
  putStrLn "Enter file path:"
  path <- getLine
  freqMap <- readFileAndCount path
  printFrequencies freqMapmodule WordFrequency where

import Data.Char (toLower, isAlpha)
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
    cleanWord = map toLower . filter isAlpha
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countHelper word rest

formatOutput :: [WordCount] -> String
formatOutput counts = unlines $ map formatPair counts
  where
    formatPair (word, count) = word ++ ": " ++ show count

processFile :: FilePath -> IO ()
processFile filename = do
    content <- readFile filename
    let frequencies = countWords content
    putStrLn $ formatOutput $ take 10 frequencies

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> processFile filename
        _ -> putStrLn "Usage: wordfreq <filename>"