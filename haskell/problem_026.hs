import Data.List

divPart a b = (c `div` b, c `mod` b, zerosAdded)
  where
    (c, zerosAdded) = f a 0
      where
        f n z
          | n >= b = (n, z)
          | otherwise = f (n * 10) (z + 1)

unitFractionDigits n = f 1 n
  where
    f 0 _ = []
    f x y = zs ++ [a] ++ f b n
      where
        (a, b, z) = divPart x y
        zs = take (z - 1) . repeat $ 0

limit = 1000

isCycleFor d xs ys = all (\(a,b) -> a == b) . zip (cycle xs) $ drop d ys

findCycles xs = [cs | len <- [1..999], d <- [0..20], let cs = take len . drop d $ xs, isCycleFor d cs xs]

cycleLength xs
  | (/= limit) . length . take limit $ xs = 0
  | otherwise = case findCycles . take limit $ xs of
      x:xs -> length x

main = putStrLn . show . fst . maximumBy (\(_,a) (_,b) -> compare a b) $ [(n,c) | n <- [1..999], let digits = unitFractionDigits n, let c = cycleLength digits]
