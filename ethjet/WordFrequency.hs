module WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord w = Map.insertWith (+) (normalize w) 1
    normalize = map toLower . filter isAlpha

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

displayFrequency :: [(String, Int)] -> String
displayFrequency = unlines . map (\(w, c) -> w ++ ": " ++ show c)

analyzeText :: Int -> String -> String
analyzeText n = displayFrequency . topNWords nmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let words' = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        grouped = group $ sort words'
    in sortOn (Down . snd) $ map (\ws -> (head ws, length ws)) grouped

printHistogram :: [WordCount] -> IO ()
printHistogram counts = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ replicate count '*') counts

main :: IO ()
main = do
    input <- getContents
    let frequencies = countWords input
    printHistogram frequenciesmodule WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (isAlpha, toLower)

countWords :: String -> Map.Map String Int
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
    in Map.fromListWith (+) [(w, 1) | w <- wordsList]
  where
    cleanWord = map toLower . filter isAlpha

displayFrequencies :: Map.Map String Int -> String
displayFrequencies freqMap =
    unlines [word ++ ": " ++ show count | (word, count) <- Map.toList freqMap]

processText :: String -> String
processText = displayFrequencies . countWordsmodule WordFrequency where

import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        grouped = foldr countHelper [] wordsList
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = map toLower . filter isAlpha
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : word `countHelper` rest

displayCounts :: [WordCount] -> String
displayCounts counts = unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

main :: IO ()
main = do
    let sampleText = "Hello world! This is a test. Hello again world. Test test test."
    putStrLn "Top 10 word frequencies:"
    putStrLn $ displayCounts $ countWords sampleText