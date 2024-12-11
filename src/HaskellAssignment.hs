module HaskellAssignment where

data Found = Match Int | NoMatch deriving (Eq)

instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"

------------------------------------------------
-- findFirst
------------------------------------------------

findFirst :: (Eq a) => (a -> Bool) -> [a] -> Found
findFirst needle haystack = doFindFirst needle haystack 0

doFindFirst _ [] _ = NoMatch
doFindFirst needle (x : xs) index = if (needle x) then Match index else doFindFirst needle xs (index + 1)

------------------------------------------------
-- palindrome
------------------------------------------------
palindrome :: [Char] -> Bool
palindrome s = s == (reverse s)
