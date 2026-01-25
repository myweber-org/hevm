module WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified System.IO as IO

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words . normalize
  where
    normalize = map Char.toLower . filter (\c -> Char.isAlpha c || Char.isSpace c)
    incrementWord word = Map.insertWith (+) word 1

readAndCount :: FilePath -> IO WordCount
readAndCount path = do
    content <- IO.readFile path
    return $ countWords content

printWordCounts :: WordCount -> IO ()
printWordCounts wc = do
    let sorted = List.sortOn (\(_, count) -> negate count) $ Map.toList wc
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) sorted

main :: IO ()
main = do
    putStrLn "Enter file path:"
    path <- getLine
    counts <- readAndCount path
    printWordCounts counts