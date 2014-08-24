
main = do
    putStrLn "Hello, world!"
    putStrLn "What is your name?"
    name <- getLine
    putStrLn ("How are you today? " ++ name)


