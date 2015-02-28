sumOfSquares :: [Int] -> Int
sumOfSquares = sum . map (^ 2)

squareOfSums :: [Int] -> Int
squareOfSums nums = sum nums ^ 2

euler6 :: Int
euler6 = squareOfSums list - sumOfSquares list
    where list = [1..100]

main :: IO ()
main = print euler6
