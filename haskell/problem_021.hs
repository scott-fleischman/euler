import Data.List

properDivisors n = [x | x <- [1..n-1], n `mod` x == 0]
d = sum . properDivisors
amicablePairsTo n = [(x, y) | x <- [1..n-1], y <- [1..n-1], x /= y, d x == y, d y == x]

amicableNumbersTo n = nub . sort . foldr (\(x, y) a -> x : y : a) [] $ amicablePairsTo n

main = putStrLn . show . sum . amicableNumbersTo $ 10000
