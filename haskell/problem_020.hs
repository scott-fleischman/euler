import Data.List

factorial n = foldr (*) 1 [2..n]

digit 0 = Nothing
digit n = Just (n `mod` 10, n `div` 10)

digits = reverse . unfoldr digit

main = putStrLn . show . sum . digits . factorial $ 100