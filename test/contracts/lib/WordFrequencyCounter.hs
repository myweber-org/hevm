module WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified System.Environment as Env

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . extractWords
  where
    extractWords = words . map normalizeChar
    normalizeChar c
      | Char.isAlpha c = Char.toLower c
      | otherwise = ' '
    
    incrementWord word = Map.insertWith (+) word 1

formatResults :: WordCount -> String
formatResults = unlines . map formatEntry . List.sortOn snd . Map.toList
  where
    formatEntry (word, count) = word ++ ": " ++ show count

processText :: String -> String
processText = formatResults . countWords

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [] -> do
      putStrLn "Reading from stdin..."
      content <- getContents
      putStrLn $ processText content
    [filename] -> do
      content <- readFile filename
      putStrLn $ processText content
    _ -> putStrLn "Usage: wordfreq [filename] (reads from stdin if no filename)"