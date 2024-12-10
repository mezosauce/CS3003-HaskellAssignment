module HaskellAssignment where

data Found = Match Int | NoMatch deriving (Eq)

instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"

------------------------------------------------
-- findFirst
------------------------------------------------

findFirst :: (Eq a) => (a -> Bool) -> [a] -> Found
findFirst needle haystack = findFirstHelper needle haystack 0
  where
    findFirstHelper :: (a -> Bool) -> [a] -> Int -> Found
    findFirstHelper _ [] _ = NoMatch
    findFirstHelper needle (x : xs) index
      | needle x = Match index
      | otherwise = findFirstHelper needle xs (index + 1)

------------------------------------------------
-- palindrome
------------------------------------------------
palindrome :: [Char] -> Bool
palindrome s = s == reverse s