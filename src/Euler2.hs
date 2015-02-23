-- Unsolved until I learn generators or whatever
euler2 :: [[Int]]
euler2 = scanr (\x acc -> new acc : acc) [1,1] [10,9..1]
    where new previous = (previous !! 0) + (previous !! 1)

main :: IO ()
main = print euler2
