module WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

displayFrequencies :: [(String, Int)] -> String
displayFrequencies = unlines . map (\(w, c) -> w ++ ": " ++ show c)

analyzeText :: Int -> String -> String
analyzeText n = displayFrequencies . topNWords nmodule WordFrequency where

import qualified Data.Map as Map
import Data.Char (toLower, isAlphaNum)
import Data.List (sortBy)
import Data.Ord (comparing)

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlphaNum

readFileAndCount :: FilePath -> IO WordCount
readFileAndCount path = do
    content <- readFile path
    return $ countWords content

printWordCounts :: WordCount -> IO ()
printWordCounts wc = do
    let sorted = sortBy (flip $ comparing snd) $ Map.toList wc
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) sorted

main :: IO ()
main = do
    putStrLn "Enter file path:"
    path <- getLine
    counts <- readFileAndCount path
    printWordCounts counts