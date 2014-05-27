import Data.List

digit :: Integer -> Maybe (Integer, Integer)
digit 0 = Nothing
digit n = Just (n `mod` 10, n `div` 10)

digits :: Integer -> [Integer]
digits = reverse . unfoldr digit

main = putStrLn . show . sum $ digits (2^1000)