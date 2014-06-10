import Data.List

digits n = unfoldr digit n
  where
    digit 0 = Nothing
    digit x = Just (x `mod` 10, x `div` 10)

main = putStrLn . show . sum $ [x | x <- [10..1000000], x == (sum . map (^5) . digits $ x)]
