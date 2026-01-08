module WordFrequency where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords text =
    let wordsList = filter (not . null) $ map normalize $ words text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList
  where
    normalize = map Char.toLower . filter Char.isAlphaNum

topWords :: Int -> String -> [(String, Int)]
topWords n text =
    take n $ List.sortBy (\(_, cnt1) (_, cnt2) -> compare cnt2 cnt1) $
    Map.toList $ countWords text

displayFrequency :: String -> IO ()
displayFrequency text = do
    putStrLn "Word Frequency Analysis:"
    putStrLn "------------------------"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) $
        topWords 10 text
    putStrLn ""module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map clean $ words text
        clean = filter isAlpha . map toLower
        counts = foldl incrementCount [] wordsList
        incrementCount acc word = 
            case lookup word acc of
                Just count -> (word, count + 1) : filter ((/= word) . fst) acc
                Nothing -> (word, 1) : acc
    in sortOn (Down . snd) counts

mostFrequentWords :: Int -> String -> [WordCount]
mostFrequentWords n = take n . countWords

displayFrequency :: [WordCount] -> String
displayFrequency counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

analyzeText :: String -> String
analyzeText text = 
    let totalWords = length $ words text
        uniqueWords = length $ countWords text
        topWords = take 5 $ countWords text
    in unlines $
        [ "Total words: " ++ show totalWords
        , "Unique words: " ++ show uniqueWords
        , "Top 5 most frequent words:"
        ] ++ map (\(w,c) -> "  " ++ w ++ " (" ++ show c ++ ")") topWords