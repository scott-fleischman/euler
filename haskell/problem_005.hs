import Data.List

primes :: [Integer]
primes = primesList [2..]
  where
    primesList :: [Integer] -> [Integer]
    primesList (x:xs) = x : (primesList $ removeMultiples x xs)

    removeMultiples :: Integer -> [Integer] -> [Integer]
    removeMultiples x = filter $ (/= 0) . (`mod` x)

primeFactorIntegers :: Integer -> [Integer]
primeFactorIntegers x = recurse x (takeWhile (<= x) primes)
  where
    recurse y [] = []
    recurse y (p:ps)
      | y `mod` p == 0 = p : (recurse (y `div` p) (p:ps))
      | otherwise = recurse y ps

data Factor = Factor { number :: Integer, power :: Integer }
  deriving (Show, Eq)

primeFactors :: Integer -> [Factor]
primeFactors = (map createFactor) . group . primeFactorIntegers
  where createFactor = \xs -> Factor { number = head xs, power = toInteger (length xs) }

smallestMultipleFactors :: [[Factor]] -> [Factor]
smallestMultipleFactors = (map maxFactor) . groupFactors . sortFactors . concat
  where
    sortFactors = sortBy (\x y -> compare (number x) (number y))
    groupFactors = groupBy (\x y -> number x == number y)
    maxFactor = maximumBy (\x y -> compare (power x) (power y))

factorsToInteger :: [Factor] -> Integer
factorsToInteger = foldl (\x y -> x * number y ^ power y) 1

smallestMultiple :: [Integer] -> Integer
smallestMultiple = factorsToInteger . smallestMultipleFactors . (map primeFactors)

main :: IO ()
main = putStrLn . show $ smallestMultiple [1..20]
