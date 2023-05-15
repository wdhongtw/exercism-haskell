module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
-- sumOfMultiples factors limit = error "You need to implement this function."
sumOfMultiples [] _ = 0
sumOfMultiples factors limit = 
    -- foldr (+) 0 candidates
    sum candidates
    where
        candidates = filter isCandidate [0..limit-1]
        isCandidate num = any (\ factor -> num `mod` factor == 0) validFactors
        validFactors = filter (/= 0) factors
