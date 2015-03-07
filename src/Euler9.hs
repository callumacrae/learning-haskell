euler9 :: Int -> Int
euler9 sum = head [ a * b * c | a <- nums, b <- nums, c <- nums, isValid a b c ]
    where nums = [1..sum]
          isValid a b c = a ^ 2 + b ^ 2 == c ^ 2 && a + b + c == sum

main :: IO ()
main = print $ euler9 1000
