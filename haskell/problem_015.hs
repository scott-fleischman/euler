factorial :: Integer -> Integer
factorial n = product [2..n]

choose n k = factorial n `div` (factorial k * factorial (n - k))

latticePathCount :: Integer -> Integer -> Integer
latticePathCount r c = choose (r + c) (max r c)
