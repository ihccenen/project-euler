sumOfSquares :: Int
sumOfSquares = sum $ [x ^ 2 | x <- [1..100]]

squareOfSum :: Int
squareOfSum = (sum $ [1..100]) ^ 2

answer :: Int
answer = squareOfSum - sumOfSquares

main :: IO ()
main = print answer
