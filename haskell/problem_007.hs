primes :: [Int]
primes = primeList [2..]
  where
    primeList (x:xs) = (:) x $ primeList $ filter (\n -> n `mod` x /= 0) xs

main = putStrLn . show $ primes !! 10000
