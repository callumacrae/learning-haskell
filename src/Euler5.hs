module Euler5 where

import Data.List
import qualified Data.Map as Map

-- Stolen from #3
smallestPrimeFactor :: Int -> Int
smallestPrimeFactor num = [ x | x <- [1..], num `mod` x == 0 ] !! 1

getPrimeFactors :: [Int] -> Int -> [Int]
getPrimeFactors factors 1 = factors
getPrimeFactors factors x = getPrimeFactors (factor : factors) (x `div` factor)
    where factor = smallestPrimeFactor x


-- #haskell is crazy and I don't understand these
-- foldl' (\z x -> M.insertWith max (head x) (length x) z) M.empty [[1], [1,1], [2,2,2], [3], [2,2]]
-- map (uncurry replicate) . nubBy (on (==) snd) . sortBy (flip compare) . map ((,) <$> length <*> head) $ [[1], [1,1], [2,2,2], [3], [2,2]]


lowestCommonMultiple :: [Int] -> Int
lowestCommonMultiple nums = foldr (\x acc -> (fst x ^ snd x) * acc) 1 primes
    where primes = map (\x -> (x, getMaxLength $ filter (firstIs x) flatPrimes)) nums
          flatPrimes = group . concat $ map (getPrimeFactors []) nums

          firstIs x y = y !! 0 == x
          getMaxLength = foldr (\x acc -> max acc (length x)) 0

main :: IO ()
main = print $ lowestCommonMultiple [2..20]
