import Data.List (maximumBy)
import Data.Ord (comparing)

limit :: Int
limit = 1000

isPrime :: Int -> Int -> Bool
isPrime n x | x < 2 = True
            | n `rem` x == 0 = False
            | otherwise = isPrime n (x - 1)

numberOfPrimes :: Int -> Int -> Int -> Int
numberOfPrimes n a b
  | isPrime x (round $ sqrt $ fromIntegral x) = numberOfPrimes (n + 1) a b
  | otherwise = n
  where
    x = abs (n ^ 2 + (a * n) + b)

answer :: (Int, Int)
answer = maximumBy (comparing snd) [(a * b, numberOfPrimes 0 a b) | a <- [- limit..limit - 1], b <- [a..limit]]

main :: IO ()
main = print answer
