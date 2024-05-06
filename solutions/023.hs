divisors :: Int -> [Int]
divisors n = [x | x <- [1..n `div` 2], n `rem` x == 0]

abundants :: [Int]
abundants = [x | x <- [12..], x < sum (divisors x)]

isSumOfAbundants :: Int -> Bool
isSumOfAbundants n = f 0 (length abundants' - 1)
  where
    abundants' = takeWhile (<= n - 12) abundants
    f x y | x > y = False
          | z > n = f x (y - 1)
          | z < n = f (x + 1) y
          | otherwise = True
      where
        z = abundants' !! x + abundants' !! y

answer :: Int
answer = sum [x | x <- [1..28123], not $ isSumOfAbundants x]

main :: IO ()
main = print answer
