smallestPrimeFactor :: Int -> Int
smallestPrimeFactor num = [ x | x <- [1..], num `mod` x == 0 ] !! 1

getPrimeFactors :: [Int] -> Int -> [Int]
getPrimeFactors factors 1 = factors
getPrimeFactors factors x = getPrimeFactors (factor : factors) (x `div` factor)
    where factor = smallestPrimeFactor x

main :: IO ()
main = print $ head $ getPrimeFactors [] 600851475143
