module HaskellAssignment where

import Control.Arrow (Arrow (first))

------------------------------------------------
-- findFirst
------------------------------------------------

data Found = Match Int | NoMatch deriving (Eq)

instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"

findFirst (first : rest) = first

findFirst :: (Eq a) => (a -> Bool) -> [a] -> Found

------------------------------------------------
-- palindrome
------------------------------------------------
palindrome

palindrome :: [Char] -> Bool
