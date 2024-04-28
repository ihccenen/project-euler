isPalindrome :: Int -> Bool
isPalindrome n = show n == reverse (show n)

answer :: Int
answer = maximum $ [x * y | x <- [100..999], y <- [100..999], isPalindrome (x * y)]

main :: IO ()
main = print answer
