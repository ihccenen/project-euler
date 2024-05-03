answer :: Int
answer = sum $ map (read . (: [])) $ show $ 2 ^ 1000

main :: IO ()
main = print answer
