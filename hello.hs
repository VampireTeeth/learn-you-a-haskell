hello :: String
hello = "Hello world"

doubleList :: (Num a) => [a] -> [a]
doubleList lst = [2*x | x <- lst]

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you are not lucky today"

addVectors :: (Integral a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "Cannot find head from an empty list"
head' (x:_) = x

main :: IO()
main = do
  putStrLn $ show (doubleList [1..10])
  putStrLn hello
  putStrLn (lucky 7)
  putStrLn (lucky 8)
  putStrLn $ show (addVectors (1,2) (3,4))
  putStrLn $ show (head' [2::Int,3,4,1,5])
  putStrLn $ show (head' []::[Int])
  putStrLn $ show (head [2,3,4,1,5])
  putStrLn $ show (head []::[Int])
  -- let l = [1 .. 10]
  -- let r = doubleList l

