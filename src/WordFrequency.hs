
module WordFrequency where

import Data.Char (toLower)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordCount = Map String Int

countWords :: [String] -> WordCount
countWords = foldr incrementWord Map.empty . concatMap words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower

topWords :: Int -> [String] -> [(String, Int)]
topWords n texts = take n $ sortOn (negate . snd) $ Map.toList $ countWords texts

displayFrequency :: [(String, Int)] -> String
displayFrequency counts = unlines $ map format counts
  where
    format (word, count) = word ++ ": " ++ show countmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))
import System.Environment (getArgs)

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = filter (all isAlpha) wordsList
        grouped = groupCount cleaned
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')
    groupCount [] = []
    groupCount (x:xs) = 
        let (matches, rest) = partition (== x) (x:xs)
        in (x, length matches) : groupCount rest

    partition _ [] = ([], [])
    partition p (y:ys)
        | p y       = let (as, bs) = partition p ys in (y:as, bs)
        | otherwise = let (as, bs) = partition p ys in (as, y:bs)

printResults :: [WordCount] -> IO ()
printResults counts = do
    putStrLn "Top 10 most frequent words:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn "Reading from stdin..."
            content <- getContents
            printResults $ countWords content
        [filename] -> do
            content <- readFile filename
            printResults $ countWords content
        _ -> putStrLn "Usage: wordfreq [filename] (reads from stdin if no filename)"