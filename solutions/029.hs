import Data.List (nub)

answer :: [Integer]
answer = nub [a ^ b | a <- [2..100], b <- [2..100]]

main :: IO ()
main = print $ length answer
