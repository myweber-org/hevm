module WordCounter where

import Data.Char (isSpace)
import Data.List (groupBy)

countWords :: String -> Int
countWords = length . filter (not . all isSpace) . groupBy (\a b -> not (isSpace a && isSpace b))

testCountWords :: Bool
testCountWords = and
    [ countWords "" == 0
    , countWords "hello" == 1
    , countWords "hello world" == 2
    , countWords "  hello   world  " == 2
    , countWords "multiple   spaces   between" == 3
    , countWords "line1\nline2\nline3" == 3
    ]