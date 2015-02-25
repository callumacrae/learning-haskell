import Data.List

euler4 :: Int
euler4 = last $ sort [ x * y | x <- bigList, y <- bigList, isPalendrome (x * y) ]
    where bigList = [999,998..900]
          isPalendrome num = show num == (reverse . show) num

main :: IO ()
main = print euler4
