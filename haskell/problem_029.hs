import Data.List

powers n = [a^b | a <- [2..n], b <- [2..n]]

main = putStrLn . show . length . nub . powers $ 100