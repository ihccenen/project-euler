answer :: Int
answer = sum $ 2 : [x | x <- [3, 5..2000000 - 1], let a = sqrt $ fromIntegral x, null $ take 1 [y | y <- [2..round a], x `rem` y == 0]]

main :: IO ()
main = print answer
