sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve [y | y <- xs, rem y x /= 0]

answer :: Int
answer = last $ take 10001 $ 2 : sieve [3, 5..]

main :: IO ()
main = print answer
