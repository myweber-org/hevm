module WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (isAlpha, toLower)

countWords :: String -> Map.Map String Int
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList
  where
    cleanWord = map toLower . filter isAlpha

printWordFrequencies :: String -> IO ()
printWordFrequencies text = do
    let freqMap = countWords text
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) 
          (Map.toList freqMap)module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map clean $ words text
        cleaned = map toLower <$> wordsList
        counts = foldr (\word acc -> 
            case lookup word acc of
                Just n -> (word, n+1) : filter ((/= word) . fst) acc
                Nothing -> (word, 1) : acc) [] cleaned
    in sortOn (Down . snd) counts
    where
        clean = filter isAlpha

formatOutput :: [WordCount] -> String
formatOutput counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

processText :: String -> String
processText = formatOutput . countWords

main :: IO ()
main = do
    putStrLn "Enter text to analyze word frequency:"
    input <- getContents
    putStrLn "\nWord frequencies:"
    putStrLn $ processText input