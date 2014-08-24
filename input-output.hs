
import Data.Char
import Control.Monad

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

reverseWordsIO :: IO ()
reverseWordsIO = do
    line <- getLine
    if null line 
      then return ()
      else do
        putStrLn $ reverseWords line
        reverseWordsIO

basicIO :: IO ()
basicIO = do 
    putStrLn "Hello, world!"
    putStrLn "What is your name?"
    name <- getLine
    putStrLn ("How are you today? " ++ name)
    putStrLn "What is your first name?"
    firstName <- getLine
    putStrLn "What is your last name?"
    lastName <- getLine
    let upperFirstName = map toUpper firstName
        upperLastName = map toUpper lastName
    putStrLn $ "Your upper full name is: " ++ upperFirstName ++ " " ++ upperLastName


demoReturn :: IO ()
demoReturn = do
    return "HAHA"
    return ()
    line <- getLine
    return 4
    putStrLn line

putCharRecursively :: IO ()
putCharRecursively = do
    c <- getChar
    if c /= ' '
      then do
        putChar c
        putCharRecursively
      else return ()

putGetCharWithWhen :: IO()
putGetCharWithWhen = do
    c <- getChar
    when (c /= ' ') $ do
      putChar c
      putGetCharWithWhen

sequenceDemo :: (Show a) => IO a -> IO ()
sequenceDemo x = do 
  rs <- sequence [x,x,x]
  print rs

mapMDemo :: IO ()
mapMDemo = do
    colors <- mapM (\a -> do
           putStrLn $ "What color for number " ++ show a ++ "?"
           color <- getLine
           return color
         ) [1,2,3,4]
    print colors

main = do
    -- putStrLn "Basic IO actions play"
    -- basicIO
    -- putStrLn "Reverse all words in a line (input empty line to quit)"
    -- reverseWordsIO
    -- putStrLn "Demostration of return"
    -- r <- demoReturn
    -- print r
    putStrLn "Demostration of getChar and putChar"
    putCharRecursively
    putGetCharWithWhen
    putStrLn "Demostration of sequence"
    sequenceDemo getLine
    putStrLn "Demostration of mapM"
    mapMDemo


