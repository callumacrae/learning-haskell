euler1 :: Int
euler1 = sum [x | x <- [1..999], x `divisibleBy` 3 || x `divisibleBy` 5]
    where x `divisibleBy` n = x `mod` n == 0

main :: IO ()
main = print euler1
