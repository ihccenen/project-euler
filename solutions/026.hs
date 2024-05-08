import Data.List (maximumBy)
import Data.Ord (comparing)

getRecurringCycle :: Int -> Int -> [Int] -> Int
getRecurringCycle denominator curr remainders
  | curr' == 0 = 0
  | curr == curr' = 1
  | any (== curr') remainders = 1
  | otherwise = 1 + getRecurringCycle denominator curr' (curr : remainders) 
  where
    (digit, curr') = (curr * 10) `divMod` denominator

answer :: (Int, Int)
answer = maximumBy (comparing snd) [(x, remainders) | x <- [1..1000], let remainders = getRecurringCycle x 1 []]

main :: IO ()
main = print answer
