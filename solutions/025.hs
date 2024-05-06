fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

answer :: Int
answer = length $ takeWhile ((< 1000) . length . show) fibs

main :: IO ()
main = print answer
