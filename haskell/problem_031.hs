coins :: [Int]
coins = [200, 100, 50, 20, 10, 5, 2, 1]

combMatch :: Int -> [[Int]] -> [Int] -> [[Int]]
combMatch 0 as _ = as
combMatch n as [] = []
combMatch n as cs@(c:cs')
	| n < c = combMatch n as cs'
	| otherwise = (combMatch n' (map (c:) as) cs) ++ combMatch n as cs' where
		n' = n - c

main = putStrLn . show . length $ combMatch 200 [[]] coins
