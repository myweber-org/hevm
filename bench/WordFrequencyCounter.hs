module WordFrequencyCounter where

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
formatOutput = T.unlines . map (\(word, count) -> T.concat [word, T.pack ": ", T.pack (show count)])

processText :: T.Text -> T.Text
processText = formatOutput . sortByFrequency . countWords

main :: IO ()
main = do
    input <- TIO.getContents
    TIO.putStrLn $ processText inputmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        grouped = foldr countWord [] wordsList
    in sortOn (Down . snd) grouped
  where
    cleanWord = map toLower . filter isAlpha
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

formatResults :: [WordCount] -> String
formatResults counts = unlines $ map formatLine counts
  where
    formatLine (word, count) = word ++ ": " ++ show count

processText :: String -> String
processText = formatResults . countWords