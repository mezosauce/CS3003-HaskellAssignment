module HaskellAssignment where

data Found = Match Int | NoMatch deriving (Eq)

instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"

------------------------------------------------
-- findFirst:
-- Take the "needle" -> the number in the list that you are trying to find
-- Take the "haystack" -> The list of element being traversed through to find the element recursivley
-- The motion of the function is to use Post-Recrusion to ensure no over memory usage on the stack and to find the element faster
------------------------------------------------

findFirst :: (Eq a) => (a -> Bool) -> [a] -> Found
findFirst needle haystack = doFindFirst needle haystack 0

doFindFirst _ [] _ = NoMatch
doFindFirst needle (x : xs) index = if (needle x) then Match index else doFindFirst needle xs (index + 1)

------------------------------------------------
-- palindrome:
-- Take the string S and compare it to its built-in reverse string. Theres really not much to HASKELL about this function urur.
------------------------------------------------
palindrome :: [Char] -> Bool
palindrome s = s == (reverse s)
