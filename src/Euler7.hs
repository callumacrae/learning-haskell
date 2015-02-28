import Primes

-- This is really inefficient lol
euler7 :: Int
euler7 = [ x | x <- [1..], isPrime x ] !! 10000

main :: IO ()
main = print euler7
