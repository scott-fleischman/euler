isLeapYear :: Int -> Bool
isLeapYear y = y `mod` 4 == 0 && (y `mod` 100 /= 0 || y `mod` 400 == 0)

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
  deriving (Eq, Show, Enum, Bounded)

getDayCount :: Int -> Month -> Int
getDayCount y m
  | elem m [Sep, Apr, Jun, Nov] = 30
  | m == Feb && isLeapYear y = 29
  | m == Feb = 28
  | otherwise = 31

data DayOfWeek = Sun | Mon | Tue | Wed | Thu | Fri | Sat
  deriving (Eq, Show, Enum, Bounded)

nextWrap :: (Eq a, Enum a, Bounded a) => a -> a
nextWrap x
  | x == maxBound = minBound
  | otherwise = succ x

data Date = Date
  {
    year :: Int,
    month :: Month,
    day :: Int
  }
  deriving (Eq, Show)

nextDate :: Date -> Date
nextDate (Date y m d) = Date ny nm nd
  where
    mDays = getDayCount y m
    nd
      | d == mDays = 1
      | otherwise = d + 1
    nm
      | nd == 1 = nextWrap m
      | otherwise = m
    ny
      | d == mDays && nm == minBound = y + 1
      | otherwise = y

next :: (Date, DayOfWeek) -> (Date, DayOfWeek)
next (d, dow) = (nextDate d, nextWrap dow)

datesStarting1900 = iterate next (Date 1900 Jan 1, Mon)
datesStarting19Cent = dropWhile (\x -> fst x /= Date 1901 Jan 1) datesStarting1900
dates20Cent = takeWhile (\x -> fst x /= Date 2001 Jan 1) datesStarting19Cent
sundaysFirstDay20Cent = filter (\x -> snd x == Sun && day (fst x) == 1) dates20Cent 

main :: IO ()
main = putStrLn $ show $ length sundaysFirstDay20Cent
