answer :: Int
answer = sum [read [x] | x <- show $ product [1..100]]

main :: IO ()
main = print answer
