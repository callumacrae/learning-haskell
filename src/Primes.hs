module Primes (
    smallestPrimeFactor, isPrime, getPrimeFactors
) where

smallestPrimeFactor :: Int -> Int
smallestPrimeFactor num = [ x | x <- [1..], num `mod` x == 0 ] !! 1

isPrime :: Int -> Bool
isPrime 1 = False
isPrime num = smallestPrimeFactor num == num

getPrimeFactors :: [Int] -> Int -> [Int]
getPrimeFactors factors 1 = factors
getPrimeFactors factors x = getPrimeFactors (factor : factors) (x `div` factor)
    where factor = smallestPrimeFactor x
