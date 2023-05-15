module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard p = keep (not . p) 
-- discard _ [] =  []
-- discard p (y:ys)
--   | p y = discard p ys
--   | otherwise = y : discard p ys

keep :: (a -> Bool) -> [a] -> [a]
keep _ [] =  []
keep p (y:ys)
  | p y = y : keep p ys
  | otherwise = keep p ys
