fibs :: Int -> [Int]
fibs x | x < 1 = []
       | otherwise = go 1 2
  where
    go y z
      | z >= x = [y]
      | otherwise = if even y then y : go z (y + z) else go z (y + z)

answer :: Int
answer = sum $ fibs 4000000

main :: IO ()
main = print answer
