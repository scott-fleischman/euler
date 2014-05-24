primes = 2 : [x | x <- [3..], isPrime x]

isPrime x = all (\p -> x `mod` p /= 0) primesToCheck
  where
    primesToCheck = takeWhile (\n -> n^2 <= x) primes

main = putStrLn . show $ sum . (takeWhile (<2000000)) $ primes
