module WordFrequencyCounter where

import qualified Data.Map as Map
import Data.Char (toLower, isAlphaNum)

countWords :: String -> Map.Map String Int
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList
  where
    cleanWord = map toLower . filter isAlphaNum

printFrequencies :: Map.Map String Int -> IO ()
printFrequencies freqMap = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) 
          (Map.toList freqMap)

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is fun, isn't it?"
    let frequencies = countWords sampleText
    printFrequencies frequencies