module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs = do
  onlyDigit <- Just $ filter isDigit xs
  localNumbers <- stripCountry onlyDigit
  validResult <- isLen localNumbers 10
  final <- Just validResult >>= onlyValid 0 >>= onlyValid 3
  Just final

stripCountry :: String -> Maybe String
stripCountry [] = Nothing
stripCountry (x : xs)
  | x == '1' = Just xs
  | otherwise = Just (x : xs)

onlyValid :: Int -> String -> Maybe String
onlyValid idx xs = do
  validLength <- toMaybe xs (\value -> idx < length value)
  toMaybe validLength (\value -> value !! idx `elem` ['2' .. '9'])

isLen :: String -> Int -> Maybe String
isLen value size
  | length value == size = Just value
  | otherwise = Nothing

toMaybe :: a -> (a -> Bool) -> Maybe a
toMaybe a check
  | check a = Just a
  | otherwise = Nothing
