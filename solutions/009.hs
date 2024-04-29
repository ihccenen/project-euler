pythagoreanTripletProduct :: Int -> Maybe Int
pythagoreanTripletProduct n =
  case take 1 tripletProducts of
    [] -> Nothing
    (x:_) -> Just x
  where
    tripletProducts = [a * b * c | a <- [1..n], b <- [a + 1..n], let c = n - (a + b), (a ^ 2 + b ^ 2) == c ^ 2 && (a + b + c) == n]

answer :: Maybe Int
answer = pythagoreanTripletProduct 1000

main :: IO ()
main = print answer
