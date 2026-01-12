module WordFrequency where

import qualified Data.Map as Map
import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type FrequencyMap = Map.Map T.Text Int

countWords :: T.Text -> FrequencyMap
countWords text = Map.fromListWith (+) [(normalize word, 1) | word <- T.words text]
  where
    normalize = T.filter isAlphaNum . T.map toLower

getTopWords :: Int -> FrequencyMap -> [(T.Text, Int)]
getTopWords n = take n . sortOn (negate . snd) . Map.toList

processFile :: FilePath -> IO ()
processFile filePath = do
    content <- TIO.readFile filePath
    let freqMap = countWords content
        topWords = getTopWords 10 freqMap
    
    putStrLn "Top 10 most frequent words:"
    mapM_ (\(word, count) -> TIO.putStrLn $ T.concat [word, T.pack ": ", T.pack (show count)]) topWords

main :: IO ()
main = processFile "input.txt"