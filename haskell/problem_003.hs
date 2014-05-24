primes :: [Integer]
primes = primesList [2..]
  where
    primesList :: [Integer] -> [Integer]
    primesList (x:xs) = x : (primesList $ filter (\n -> n `mod` x /= 0) xs)

primeFactors :: Integer -> [Integer]
primeFactors x = getFactors x primes
  where
    getFactors 1 _ = []
    getFactors y (p:ps)
      | y `mod` p == 0 = p : (getFactors (y `div` p) (p:ps))
      | otherwise = getFactors y ps

main :: IO ()
main = putStrLn . show . maximum $ primeFactors 600851475143
