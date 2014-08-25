import Control.Monad
import Data.Char

-- main = forever $ do
--   putStrLn "Get me some text"
--   l <- getLine
--   putStrLn $ map toUpper l

main = do
    contents <- getContents
    putStr $ map toUpper contents
