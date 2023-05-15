module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.Map (Map, adjust, fromList)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
-- nucleotideCounts xs = error "You need to implement this function."
nucleotideCounts [] = Right $ fromList [(A, 0), (C, 0), (G, 0), (T, 0)]
nucleotideCounts (x : xs) =
  case result of
    Left msg -> Left msg
    Right nextResult -> case x of
      'A' -> Right $ adjust (+ 1) A nextResult
      'C' -> Right $ adjust (+ 1) C nextResult
      'G' -> Right $ adjust (+ 1) G nextResult
      'T' -> Right $ adjust (+ 1) T nextResult
      _ -> Left "unknown character"
  where
    result = nucleotideCounts xs
