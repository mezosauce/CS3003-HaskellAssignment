module HaskellAssignment where
import Control.Exception (NonTermination(NonTermination))

data Found = Match Int | NoMatch deriving (Eq)

instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"

------------------------------------------------
-- findFirst
------------------------------------------------

findFirst :: (Eq a) => (a -> Bool) -> [a] -> Found
findFirst needle haystack = doFindFirst needle stack 0
doFindFirst _[]_ = NoMatch
doFindFirst needle (x:xs) index = if (needle x) then Match otherwise NoMatch

------------------------------------------------
-- palindrome
------------------------------------------------
palindrome :: [Char] -> Bool
palindrome s = s == (rvrs s)