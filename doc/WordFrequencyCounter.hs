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
            putStrLn $ formatResults counts