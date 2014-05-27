import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List

triangles = map fst $ make (1,1)
  where
    make (a,b) = (a,b) : make (a+b+1, b+1)

addOrUpdate :: Integer -> Maybe [Integer] -> Maybe [Integer]
addOrUpdate x Nothing = Just [x]
addOrUpdate x (Just y) = Just $ x : y

addNewMultiple x = Map.alter (addOrUpdate $ x + x) x

updateMultiples x m = foldr addNewMultiple (Map.delete x m) (fromJust $ Map.lookup x m)

primes = 2 : sieve Map.empty [3,5..]
  where
    sieve m (x:xs)
      | Map.member x m = sieve (updateMultiples x m) xs
      | otherwise = x : sieve (addNewMultiple x m) xs

factors n = f n primes
  where
    f 1 _ = []
    f y (x:xs)
      | y `mod` x == 0 = x : f (y `div` x) (x:xs)
      | otherwise = f y xs

factorCount n = length . Data.List.nub . Data.List.sort . map product . Data.List.subsequences $ factors n

main = putStrLn . show . take 1 $ [x | x <- triangles, factorCount x > 500]
