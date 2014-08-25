
main = interact palindrome

palindrome :: String -> String
palindrome input = unlines (map (\line -> 
                           if isPalindrome line then "Palindrome" else "Not palindrome") 
                           (lines input))

betterPalindrome :: String -> String
betterPalindrome = unlines . (map (\l -> if isPalindrome l then "Palindrome" else "Not palindrome")) . lines

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

