module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
-- anagramsFor xs xss = error "You need to implement this function."
anagramsFor xs =
  filter $ checkFunc xs
  where
    checkFunc a b =
      toLowerString a /= toLowerString b && canonical a == canonical b
    canonical = sort . toLowerString
    toLowerString = fmap toLower
