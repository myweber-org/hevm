module WordFrequency where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        freqMap = foldr (\word acc -> 
            case lookup word acc of
                Just count -> (word, count + 1) : filter ((/= word) . fst) acc
                Nothing -> (word, 1) : acc
            ) [] wordsList
    in sortOn (Down . snd) freqMap
  where
    cleanWord = map toLower . filter isAlphaNum

printWordFrequencies :: [WordCount] -> IO ()
printWordFrequencies counts = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is functional. World says hello."
    let frequencies = countWords sampleText
    printWordFrequencies frequencies