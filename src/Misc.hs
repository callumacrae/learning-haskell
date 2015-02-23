import Data.Char

replicate' :: Int -> a -> [a]
replicate' n x
    | n == 0    = []
    | otherwise = x : replicate' (n - 1) x

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : (repeat' x)

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):(zip' xs ys)

elem' :: (Eq a) => a -> [a] -> Bool
elem' n [] = False
elem' n (x:xs)
    | n == x    = True
    | otherwise = elem' n xs

collatz :: Int -> [Int]
collatz 1 = [1]
collatz x
    | even x = x : collatz (x `div` 2)
    | odd x  = x : collatz (x * 3 + 1)

numberOf :: Int
numberOf = length (filter isLong [1..100])
    where isLong x = length (collatz x) > 15

strangeCase :: String -> String
strangeCase [] = []
strangeCase (x:xs)
    | even $ length xs = toUpper x : strangeCase xs
    | otherwise        = toLower x : strangeCase xs

addSpaces :: String -> String
addSpaces (x:xs) = x : foldr (\x acc -> ' ' : x : acc) [] xs