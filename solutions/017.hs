answer :: Int
answer = length $ [1..1000] >>= f
  where
    f x = case show x of
      "0" -> ""
      "1" -> "one"
      "2" -> "two"
      "3" -> "three"
      "4" -> "four"
      "5" -> "five"
      "6" -> "six"
      "7" -> "seven"
      "8" -> "eight"
      "9" -> "nine"
      [x, y] -> case x of
        '0' -> f (read [y])
        '1' -> case y of
          '0' -> "ten"
          '1' -> "eleven"
          '2' -> "twelve"
          '3' -> "thirteen"
          '5' -> "fifteen"
          '8' -> "eighteen"
          _ -> f (read [y]) <> "teen"
        '2' -> case y of
          '0' -> "twenty"
          _ -> "twenty" <> f (read [y])
        '3' -> case y of
          '0' -> "thirty"
          _ -> "thirty" <> f (read [y])
        '4' -> case y of
          '0' -> "forty"
          _ -> "forty" <> f (read [y])
        '5' -> case y of
          '0' -> "fifty"
          _ -> "fifty" <> f (read [y])
        '8' -> case y of
          '0' -> "eigthy"
          _ -> "eigthy" <> f (read [y])
        _ -> case y of
          '0' -> f (read [x]) <> "ty"
          _ -> f (read [x]) <> "ty" <> f (read [y])
      [x, '0', '0'] -> f (read [x]) <> "hundred"
      [x, y, k] -> f (read [x]) <> "hundredand" <> f (read [y, k])
      "1000" -> "onethousand"
      _ -> ""

main :: IO ()
main = print answer
