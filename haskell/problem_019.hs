import Data.List

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
  deriving (Eq, Enum, Bounded, Show)

succWrap :: (Eq a, Enum a, Bounded a) => a -> a
succWrap x
  | x == maxBound = minBound
  | otherwise = succ x

data Date = Date { year :: Int, month :: Month, day :: Int }
  deriving (Eq, Show)

isLeapYear :: Int -> Bool
isLeapYear x = x `mod` 4 == 0 && (x `mod` 100 /= 0 || x `mod` 400 == 0)

daysInMonth :: Int -> Month -> Int
daysInMonth _ Sep = 30
daysInMonth _ Apr = 30
daysInMonth _ Jun = 30
daysInMonth _ Nov = 30
daysInMonth y Feb
  | isLeapYear y = 29
  | otherwise = 28
daysInMonth _ _ = 31

nextDay :: Date -> Date
nextDay x
  | daysInMonth (year x) (month x) /= day x = x { day = 1 + day x }
  | month x /= maxBound = x { month = succ (month x), day = 1 }
  | otherwise = Date { year = 1 + year x, month = minBound, day = 1 }

data DayOfWeek = Sun | Mon | Tue | Wed | Thu | Fri | Sat
  deriving (Eq, Enum, Bounded, Show)

nextPair :: (Date, DayOfWeek) -> (Date, DayOfWeek)
nextPair (x, y) = (nextDay x, succWrap y)

days :: (Date, DayOfWeek) -> [(Date, DayOfWeek)]
days x = x : days (nextPair x)

main = putStrLn . show . length .
  filter (\x -> day (fst x) == 1 && snd x == Sun) .
  takeWhile (\x -> year (fst x) <= 2000) .
  dropWhile (\x -> year (fst x) == 1900 ) $
  days (Date { year = 1900, month = Jan, day = 1 }, Mon)
