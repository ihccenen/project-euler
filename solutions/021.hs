d :: Int -> Int
d n = sum [x | x <- [1..n `div` 2], n `rem` x == 0]

answer ::Int
answer = sum [x + y | x <- [1..9999], let y = d x, x == d y, x > y]

main :: IO ()
main = print answer
