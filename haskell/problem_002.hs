fibs :: [Int]
fibs = 1 : 1 : (nextFibs 1 1)
  where
    nextFibs x y = nextFib : (nextFibs y nextFib)
      where nextFib = x + y

main = putStrLn . show $ sum [x | x <- ((takeWhile (<= 4000000)) . (drop 1)) fibs, even x]
