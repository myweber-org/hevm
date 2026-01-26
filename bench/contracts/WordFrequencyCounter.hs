module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))
import System.Environment (getArgs)

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let words' = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        freqMap = foldr (\w m -> case lookup w m of
                                  Just count -> (w, count + 1) : filter ((/= w) . fst) m
                                  Nothing -> (w, 1) : m) [] words'
    in sortOn (Down . snd) freqMap

formatOutput :: [WordCount] -> String
formatOutput counts = unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

processFile :: FilePath -> IO ()
processFile filename = do
    content <- readFile filename
    putStrLn $ formatOutput $ countWords content

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> processFile filename
        _ -> putStrLn "Usage: wordfreq <filename>"module WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type WordCount = Map.Map T.Text Int

countWords :: T.Text -> WordCount
countWords = Map.fromListWith (+) . map (\w -> (w, 1)) . filter (not . T.null) . map normalize . T.words
  where
    normalize = T.toLower . T.filter Char.isLetter

sortByFrequency :: WordCount -> [(T.Text, Int)]
sortByFrequency = List.sortBy (\(_, c1) (_, c2) -> compare c2 c1) . Map.toList

formatOutput :: [(T.Text, Int)] -> T.Text
formatOutput = T.unlines . map (\(word, count) -> T.pack (show count) <> " " <> word)

processText :: T.Text -> T.Text
processText = formatOutput . sortByFrequency . countWords

main :: IO ()
main = do
    input <- TIO.getContents
    TIO.putStr $ processText inputmodule WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords text =
    let wordsList = filter (not . null) $ map normalize $ splitText text
    in List.foldl' incrementWord Map.empty wordsList
  where
    splitText = words . map (\c -> if Char.isAlpha c then Char.toLower c else ' ')
    normalize = filter Char.isAlpha
    
    incrementWord :: WordCount -> String -> WordCount
    incrementWord counts word = Map.insertWith (+) word 1 counts

getTopWords :: Int -> WordCount -> [(String, Int)]
getTopWords n counts =
    take n $ List.sortBy (\(_, a) (_, b) -> compare b a) $ Map.toList counts

printWordFrequencies :: [(String, Int)] -> IO ()
printWordFrequencies frequencies =
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) frequencies

analyzeText :: String -> IO ()
analyzeText text = do
    let counts = countWords text
    let topWords = getTopWords 10 counts
    putStrLn "Top 10 most frequent words:"
    printWordFrequencies topWords
    putStrLn $ "\nTotal unique words: " ++ show (Map.size counts)