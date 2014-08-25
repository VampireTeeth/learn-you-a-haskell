
main = do
    contents <- getContents
    putStrLn "shortLineOnly"
    putStr (shortLinesOnly contents)
    putStrLn "shortLineOnlyAgain"
    putStr (shortLinesOnlyAgain contents)


shortLinesOnly :: String -> String
shortLinesOnly contents = 
    let allLines = lines contents
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in result

shortLinesOnlyAgain :: String -> String
shortLinesOnlyAgain input = unlines shortLines where
  shortLines = filter (\line -> length line < 10) allLines
  allLines = lines input
