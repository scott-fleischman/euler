import Data.List
import Data.Ord

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n
  | even n = n : collatz (n `div` 2)
  | otherwise = n : collatz (3 * n + 1)

main = putStrLn . show . fst . head . reverse . sortBy (comparing snd) $ [(x,len) | x <- [999999,999998..1], let len = length $ collatz x]
