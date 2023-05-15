module Prime (nth) where

-- nth n = error "You need to implement this function."
nth :: Int -> Maybe Integer
-- nth n
--   | n <= 0 = Nothing
--   | otherwise = Just ((filter isPrime $ enumFrom 2) !! (n - 1))
--      where
--        isPrime num = not (any (\ a -> mod num a == 0) (enumFromTo 2 (num -1)))
nth n
    | n <= 0 = Nothing
    | otherwise = Just (primes !! (n-1))
        where primes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x: (sieve [y | y <-xs, mod y x /= 0])
