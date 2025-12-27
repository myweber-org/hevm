module WordFrequencyCounter where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char (isAlpha, toLower)

countWords :: T.Text -> Map.Map T.Text Int
countWords text = Map.fromListWith (+) [(normalize word, 1) | word <- T.words text]
  where
    normalize = T.filter isAlpha . T.toLower

processFile :: FilePath -> IO (Map.Map T.Text Int)
processFile path = do
    content <- TIO.readFile path
    return $ countWords content

displayTopN :: Int -> Map.Map T.Text Int -> IO ()
displayTopN n freqMap = do
    let sorted = take n $ Map.toDescList freqMap
    mapM_ (\(word, count) -> putStrLn $ T.unpack word ++ ": " ++ show count) sorted

main :: IO ()
main = do
    putStrLn "Enter file path:"
    path <- getLine
    freqMap <- processFile path
    putStrLn "Enter number of top words to display:"
    n <- readLn
    displayTopN n freqMap