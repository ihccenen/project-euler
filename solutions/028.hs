limit :: Int
limit = 1000

answer :: Int
answer = sum $ go 1 2
  where
    go :: Int -> Int -> [Int]
    go n x | x > limit = [n]
           | otherwise = [n, y, z, k] <> go j (x + 2)
       where
         y = (n + x)
         z = (y + x)
         k = (z + x)
         j = (k + x)

main :: IO ()
main = print $ answer
