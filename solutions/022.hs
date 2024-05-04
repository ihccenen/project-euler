import Data.Char (ord)
import Data.List (sort)

getNameValue :: Int -> String -> Int
getNameValue pos s = pos * sum [ord x `rem` 32 | x <- s]

getName :: String -> String
getName [] = []
getName (',':_) = []
getName ('"':xs) = getName xs
getName (x:xs) = x : getName xs

splitNames :: String -> [String]
splitNames [] = []
splitNames s = name : splitNames rest
  where
    name = getName s
    rest = drop 1 $ dropWhile (/=',') s

answer :: String -> Int
answer = sum . zipWith getNameValue [1..] . sort . splitNames

main :: IO ()
main = do
  input <- readFile "./022_names.txt"
  print $ answer input
