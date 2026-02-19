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
          (Map.toList freqMap)