findLargestFactor :: Int -> Int -> Int
findLargestFactor n currFactor
  | n == 1 = currFactor
  | n `mod` currFactor == 0 = findLargestFactor (n `div` currFactor) currFactor
  | otherwise = findLargestFactor n (currFactor + 1)

answer :: Int
answer = findLargestFactor 600851475143 2

main :: IO ()
main = print answer
