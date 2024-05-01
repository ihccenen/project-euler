import Data.List (group)

factorList :: Int -> Int -> [Int]
factorList p n
  | n < 2 = []
  | n `rem` p == 0 = p : factorList p (n `div` p)
  | otherwise = factorList (p + 1) n

answer :: Int
answer = head $ dropWhile f [x * (x + 1) `div` 2 | x <- [0..]]
  where
    f = (< 500) . product . map ((+ 1) . length) . group . factorList 2

main :: IO ()
main = print answer
