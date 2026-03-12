module WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type WordFreq = Map.Map String Int

countWords :: String -> WordFreq
countWords text =
    let wordsList = filter (not . null) $ map normalize $ splitWords text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList
  where
    splitWords = words . map (\c -> if Char.isAlphaNum c then c else ' ')
    normalize = map Char.toLower

topNWords :: Int -> String -> [(String, Int)]
topNWords n text =
    take n $ List.sortBy (\(_, cnt1) (_, cnt2) -> compare cnt2 cnt1) $
    Map.toList $ countWords text

displayFrequencies :: [(String, Int)] -> String
displayFrequencies freqs =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqs

processText :: Int -> String -> String
processText n = displayFrequencies . topNWords nmodule WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type WordCount = Map.Map T.Text Int

countWords :: T.Text -> WordCount
countWords text =
    let wordsList = T.words $ T.toLower $ T.filter (\c -> Char.isAlpha c || Char.isSpace c) text
    in Map.fromListWith (+) [(w, 1) | w <- wordsList]

topNWords :: Int -> WordCount -> [(T.Text, Int)]
topNWords n wordCount =
    take n $ List.sortBy (\(_, cnt1) (_, cnt2) -> compare cnt2 cnt1) $ Map.toList wordCount

displayResults :: [(T.Text, Int)] -> T.Text
displayResults results =
    T.unlines $ map (\(word, count) -> T.concat [word, T.pack ": ", T.pack (show count)]) results

processText :: T.Text -> T.Text
processText text =
    let wordCount = countWords text
        topWords = topNWords 10 wordCount
    in displayResults topWords

main :: IO ()
main = do
    putStrLn "Enter text (press Ctrl+D on empty line to finish):"
    input <- TIO.getContents
    let result = processText input
    TIO.putStrLn $ T.concat [T.pack "\nTop 10 most frequent words:\n", result]