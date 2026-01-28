module WordCounter where

import Data.Char (isSpace)
import Data.List (words)

-- | Count the number of words in a string.
-- Words are defined as sequences of characters separated by whitespace.
countWords :: String -> Int
countWords = length . words

-- | A simple test suite for the word counter.
testWordCounter :: IO ()
testWordCounter = do
    let testCases =
            [ ("", 0)
            , ("hello", 1)
            , ("hello world", 2)
            , ("  multiple   spaces   between   words  ", 4)
            , ("line1\nline2\nline3", 3)
            , ("punctuation, like this!", 3)
            ]

    putStrLn "Running word counter tests..."
    mapM_ runTest testCases
    putStrLn "All tests passed."

  where
    runTest (input, expected) =
        let result = countWords input
        in if result == expected
            then putStrLn $ "PASS: \"" ++ input ++ "\" -> " ++ show result
            else error $ "FAIL: \"" ++ input ++ "\" expected " ++ show expected ++ " got " ++ show result

-- Example usage in GHCi:
-- countWords "Hello Haskell world"
-- testWordCountermodule WordCounter where

import Data.Char (isSpace)
import Data.List (groupBy)

countWords :: String -> Int
countWords = length . filter (not . all isSpace) . groupBy (\a b -> not (isSpace a && isSpace b))

testCountWords :: Bool
testCountWords = and
    [ countWords "" == 0
    , countWords "hello" == 1
    , countWords "hello world" == 2
    , countWords "  multiple   spaces   " == 2
    , countWords "line1\nline2\nline3" == 3
    , countWords "punctuation, works! right?" == 4
    ]