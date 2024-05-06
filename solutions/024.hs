factorial :: Int -> Int
factorial n = product [1..n]

getFirstPermNumber :: Int -> Int -> [Int] -> [Int] -> Maybe (Int, ([Int], Int))
getFirstPermNumber _ _ [] _ = Nothing
getFirstPermNumber acc limit xs@(x:_) ys
  | acc + fac > limit = Just (x, (ys', limit - acc))
  | otherwise = getFirstPermNumber (acc + fac) limit (drop 1 xs) ys
  where
    ys' = filter (/= x) ys
    fac = factorial (length ys')

getNthLexiPerm :: Int -> [Int] -> [Int]
getNthLexiPerm _ [] = []
getNthLexiPerm limit xs@(x:_) =
  case getFirstPermNumber 0 limit xs xs of
    Nothing -> []
    Just (n, (rest, newLimit)) -> n : getNthLexiPerm newLimit rest

answer :: [Int]
answer = getNthLexiPerm (1000000 - 1) [0..9]


main :: IO ()
main = print answer

