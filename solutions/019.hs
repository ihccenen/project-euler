data DayOfWeek = Sun | Mon | Tue | Wed | Thu | Fri | Sat deriving (Show, Eq, Enum)

data Month = Jan | Feb | Mar | Apr | May | June | July | Aug | Sept | Oct | Nov | Dec deriving (Show, Eq, Enum)

nextDay :: DayOfWeek -> DayOfWeek
nextDay Sat = Sun
nextDay d = succ d

nextMonth :: Month -> Month
nextMonth Dec = Jan
nextMonth m = succ m

nextFirstDayOfWeek :: Int -> DayOfWeek -> DayOfWeek
nextFirstDayOfWeek days dayOfWeek = toEnum $ (days - (fromEnum Sat - (fromEnum dayOfWeek - 1))) `rem` 7

countSun :: Int -> DayOfWeek -> Month -> Int -> Int
countSun count dayOfWeek month year
  | year == 2000 = count + count'
  | otherwise = countSun (count + count') dayOfWeek' (nextMonth month) (year + fromEnum (month == Dec))
  where
    k = 27 + fromEnum (year `rem` 4 == 0 && (year `rem` 100 /= 0 || year `rem` 400 == 0))
    count' = fromEnum $ dayOfWeek == Sun
    dayOfWeek'= case month of
      Sept -> nextFirstDayOfWeek 29 dayOfWeek
      Apr -> nextFirstDayOfWeek 29 dayOfWeek
      June -> nextFirstDayOfWeek 29 dayOfWeek
      Nov -> nextFirstDayOfWeek 29 dayOfWeek
      Feb -> nextFirstDayOfWeek k dayOfWeek
      _ -> nextFirstDayOfWeek 30 dayOfWeek

answer :: Int
answer = countSun 0 Mon Jan 1900

main :: IO ()
main = print answer
