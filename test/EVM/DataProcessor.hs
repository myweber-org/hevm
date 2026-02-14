
module DataProcessor where

import Data.List (intercalate)
import Data.List.Split (splitOn)

type CSVRow = [String]
type CSVData = [CSVRow]

parseCSV :: String -> CSVData
parseCSV content = map (splitOn ",") (lines content)

numericColumns :: CSVData -> [Int]
numericColumns rows = 
    case rows of
        [] -> []
        (header:_) -> 
            map fst $ filter (\(_, val) -> all isNumericChar val) $ zip [0..] header
    where
        isNumericChar c = c `elem` "0123456789.-"

computeColumnStats :: CSVData -> Int -> (Double, Double, Double)
computeColumnStats rows colIndex =
    let values = mapMaybe (safeRead . (!! colIndex)) rows
        count = fromIntegral $ length values
        total = sum values
        avg = total / count
        variance = sum (map (\x -> (x - avg) ** 2) values) / count
    in (total, avg, sqrt variance)
    where
        safeRead s = case reads s of
            [(x, "")] -> Just x
            _ -> Nothing

generateReport :: CSVData -> String
generateReport csvData =
    let headers = head csvData
        numericCols = numericColumns csvData
        stats = map (computeColumnStats (tail csvData)) numericCols
        reportLines = zipWith3 (\col idx (total, avg, std) ->
            "Column " ++ show idx ++ " (" ++ headers !! col ++ "): " ++
            "Sum=" ++ show total ++ ", " ++
            "Avg=" ++ show avg ++ ", " ++
            "StdDev=" ++ show std) numericCols [0..] stats
    in intercalate "\n" reportLines

processCSVFile :: FilePath -> IO String
processCSVFile path = do
    content <- readFile path
    let parsed = parseCSV content
    return $ generateReport parsed