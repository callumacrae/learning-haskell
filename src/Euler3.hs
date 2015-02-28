import Primes

main :: IO ()
main = print $ head $ getPrimeFactors [] 600851475143
