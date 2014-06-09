fibs = 1 : 1 : fib 1 1
  where
    fib n1 n2 = n3 : fib n2 n3
      where n3 = n1 + n2

indexedFibs = zip [1..] fibs

main = putStrLn . show . fst . head . dropWhile (\(i,x) -> x < 10^999) $ indexedFibs
