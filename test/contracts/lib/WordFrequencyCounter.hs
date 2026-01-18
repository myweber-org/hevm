module WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified System.Environment as Env

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words . normalize
  where
    normalize = map Char.toLower . filter (\c -> Char.isAlpha c || Char.isSpace c)
    incrementWord word = Map.insertWith (+) word 1

formatResults :: WordCount -> String
formatResults = unlines . map formatItem . List.sortOn (negate . snd) . Map.toList
  where
    formatItem (word, count) = word ++ ": " ++ show count

main :: IO ()
main = do
    args <- Env.getArgs
    case args of
        [] -> putStrLn "Usage: wordfreq <text>"
        textPieces -> do
            let text = unwords textPieces
            putStrLn "Word frequencies:"
            putStrLn $ formatResults $ countWords text