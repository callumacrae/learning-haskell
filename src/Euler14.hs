import Data.List

collatz :: Int -> [Int]
collatz 1 = [1]
collatz x
    | even x = x : collatz (x `div` 2)
    | odd x  = x : collatz (x * 3 + 1)

euler14 :: Int
euler14 = fst $ maximumBy compareSnd collatzLengths
    where compareSnd x y = compare (snd x) (snd y)
          collatzLengths = [ (x, length $ collatz x) | x <- [1..1000000] ]

main :: IO ()
main = print $ euler14
