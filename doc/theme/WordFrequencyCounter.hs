module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))
import System.Environment (getArgs)

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let words' = map normalize $ filter (not . null) $ splitWords text
        normalized = map toLower words'
        groups = groupCount normalized
    in take 10 $ reverse $ sortOn snd groups
  where
    normalize = filter isAlpha
    splitWords = words
    groupCount = map (\ws -> (head ws, length ws)) . group . sort
    group [] = []
    group (x:xs) = (x : takeWhile (== x) xs) : group (dropWhile (== x) xs)

displayResults :: [WordCount] -> IO ()
displayResults counts = do
    putStrLn "Top 10 most frequent words:"
    putStrLn "----------------------------"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

processFile :: FilePath -> IO ()
processFile filepath = do
    content <- readFile filepath
    let counts = countWords content
    displayResults counts

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filepath] -> processFile filepath
        _ -> putStrLn "Usage: wordfreq <filename>"module WordFrequencyCounter where

import Data.Char (toLower)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

countWords :: String -> Map String Int
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower

sortByFrequency :: Map String Int -> [(String, Int)]
sortByFrequency = sortOn (negate . snd) . Map.toList

wordFrequencyReport :: String -> [(String, Int)]
wordFrequencyReport = sortByFrequency . countWords

main :: IO ()
main = do
  let text = "Hello world hello Haskell world of functional programming"
  mapM_ print $ wordFrequencyReport text