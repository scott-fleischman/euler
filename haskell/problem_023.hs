import qualified Data.Set as S

divisors :: Int -> [Int]
divisors n = [x | x <- [1..n `div` 2], n `mod` x == 0]

isAbundant :: Int -> Bool
isAbundant n = (> n) . sum . divisors $ n

abundants :: [Int]
abundants = filter isAbundant $ [1..28123]

chooseWith :: (a -> a -> b) -> [a] -> [b]
chooseWith f [] = []
chooseWith f (x:xs) = (map (f x) (x:xs)) ++ (chooseWith f xs)

abundantSums = S.fromList . chooseWith (+) $ abundants

numbers = S.fromList [1..28123]

nonAbundantSums = S.difference numbers abundantSums

main = putStrLn . show . sum . S.toList $ nonAbundantSums
