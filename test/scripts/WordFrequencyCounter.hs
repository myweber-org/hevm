module WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = filter Char.isAlpha . map Char.toLower

formatResults :: WordCount -> String
formatResults = unlines . map formatEntry . List.sortOn snd . Map.toList
  where
    formatEntry (word, count) = word ++ ": " ++ show count

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: WordFrequencyCounter <text>"
    textPieces -> do
      let text = unwords textPieces
      let frequencyMap = countWords text
      putStrLn $ formatResults frequencyMap