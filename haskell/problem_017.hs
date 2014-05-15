import Data.List

onesWord :: Int -> String
onesWord 1 = "one"
onesWord 2 = "two"
onesWord 3 = "three"
onesWord 4 = "four"
onesWord 5 = "five"
onesWord 6 = "six"
onesWord 7 = "seven"
onesWord 8 = "eight"
onesWord 9 = "nine"
onesWord _ = ""

teensWord :: Int -> String
teensWord 10 = "ten"
teensWord 11 = "eleven"
teensWord 12 = "twelve"
teensWord 13 = "thirteen"
teensWord 14 = "fourteen"
teensWord 15 = "fifteen"
teensWord 16 = "sixteen"
teensWord 17 = "seventeen"
teensWord 18 = "eighteen"
teensWord 19 = "nineteen"
teensWord _ = ""

tensWord :: Int -> String
tensWord 2 = "twenty"
tensWord 3 = "thirty"
tensWord 4 = "forty"
tensWord 5 = "fifty"
tensWord 6 = "sixty"
tensWord 7 = "seventy"
tensWord 8 = "eighty"
tensWord 9 = "ninety"
tensWord _ = ""

digitize :: Int -> Int -> [Int]
digitize base x = reverse $ unfoldr expand x
  where
    expand 0 = Nothing
    expand n = Just (n `mod` base, n `div` base)

getDigit :: [Int] -> Int -> Int
getDigit digits digit
  | null ds = 0
  | otherwise = head ds
  where ds = take 1 $ drop digit $ reverse digits

whereNotNull :: [[a]] -> [[a]]
whereNotNull = filter (not . null)

intToWords :: Int -> [String]
intToWords x
  | x < 10 = [onesWord x]
  | x < 20 = [teensWord x]
  | x < 100 = tensWord tens : (whereNotNull [onesWord ones])
  | x < 1000 && x `mod` 100 /= 0 = [onesWord hundreds, "hundred", "and"] ++ (intToWords $ x `mod` 100)
  | x < 1000 = [onesWord hundreds, "hundred"]
  | x < 10000 = [onesWord thousands, "thousand"] ++ (whereNotNull $ intToWords (x `mod` 1000))
  where
    digits = digitize 10 x
    digit = getDigit digits
    ones = digit 0
    tens = digit 1
    hundreds = digit 2
    thousands = digit 3

main :: IO()
main = putStrLn . show . length . concat . concat $ map intToWords [1..1000]
