limit :: Int
limit = 20

answer :: Int
answer = f 0 0 []
  where
    g :: Int -> [Int] -> [Int]
    g _ [] = []
    g _ [_] = [1]
    g n (x:xs) = n : g n' xs
      where
        n' = n - x

    f :: Int -> Int -> [Int] -> Int
    f x n ns
      | x == 0 = f (x + 1) (limit + 1) $ replicate (limit + 1) 1
      | x == limit = sum ns
      | otherwise = f (x + 1) n' ns'
      where
        n' = sum ns
        ns' = g n' ns

main :: IO ()
main = print answer
