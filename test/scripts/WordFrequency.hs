module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordCount = Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord w = Map.insertWith (+) (normalize w) 1
    normalize = filter isAlpha . map toLower

topWords :: Int -> String -> [(String, Int)]
topWords n text = take n $ sortOn (negate . snd) $ Map.toList (countWords text)

displayFrequency :: String -> IO ()
displayFrequency text = do
  putStrLn "Word frequencies:"
  mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) (topWords 10 text)module WordFrequency where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map normalize wordsList
        grouped = groupCount cleaned
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = filter (\c -> isAlphaNum c || c == '\'')
    normalize = map toLower
    groupCount [] = []
    groupCount (w:ws) = 
        let (matches, rest) = partition (== w) ws
        in (w, 1 + length matches) : groupCount rest

    partition _ [] = ([], [])
    partition p (x:xs)
        | p x       = let (ys, zs) = partition p xs in (x:ys, zs)
        | otherwise = let (ys, zs) = partition p xs in (ys, x:zs)

displayCounts :: [WordCount] -> String
displayCounts counts = unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

main :: IO ()
main = do
    putStrLn "Enter text (press Ctrl+D when done):"
    content <- getContents
    let frequencies = countWords content
    putStrLn "\nTop 10 word frequencies:"
    putStrLn $ displayCounts frequenciesmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map normalize $ words text
        normalize = filter isAlpha . map toLower
        frequencies = foldr countWord [] wordsList
        countWord w [] = [(w, 1)]
        countWord w ((word, count):rest)
            | w == word = (word, count + 1) : rest
            | otherwise = (word, count) : countWord w rest
    in sortOn (Down . snd) frequencies

formatOutput :: [WordCount] -> String
formatOutput counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

processText :: String -> String
processText = formatOutput . countWords

main :: IO ()
main = do
    input <- getContents
    putStrLn $ processText input