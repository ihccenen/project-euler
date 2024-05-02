longest :: (Int, Int) -> (Int, Int) -> (Int, Int)
longest x@(_, xSteps) y@(_, ySteps) = if xSteps > ySteps then x else y

collatz :: Int -> Int
collatz 0 = 0
collatz 1 = 1
collatz n = (collatz $ if even n then n `div` 2 else 3 * n + 1) + 1

answer :: (Int, Int)
answer = foldr longest (0, 0) $ map (\x -> (x, collatz x)) [1..10^6]

main :: IO ()
main = print answer
