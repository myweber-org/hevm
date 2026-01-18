module WordFrequency where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords = foldr incrementWord Map.empty . words . normalize
  where
    normalize = map Char.toLower . filter (\c -> Char.isAlpha c || Char.isSpace c)
    incrementWord word = Map.insertWith (+) word 1

formatResults :: FrequencyMap -> String
formatResults = unlines . map formatEntry . List.sortOn snd . Map.toList
  where
    formatEntry (word, count) = word ++ ": " ++ show count

processText :: String -> String
processText = formatResults . countWords

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> interact processText
        [filename] -> readFile filename >>= putStrLn . processText
        _ -> putStrLn "Usage: wordfreq [filename] (omit filename for stdin)"