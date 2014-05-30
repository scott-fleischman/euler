triangle = [
  [75],
  [95, 64],
  [17, 47, 82],
  [18, 35, 87, 10],
  [20, 04, 82, 47, 65],
  [19, 01, 23, 75, 03, 34],
  [88, 02, 77, 73, 07, 63, 67],
  [99, 65, 04, 28, 06, 16, 70, 92],
  [41, 41, 26, 56, 83, 40, 80, 70, 33],
  [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
  [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
  [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
  [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
  [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
  [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]]

pairsLeft :: Num a => [a] -> [b] -> [(a, b)]
pairsLeft xs ys = zip (xs ++ [0]) ys

pairsRight :: Num a => [a] -> [b] -> [(a, b)]
pairsRight xs ys = zip (0 : xs) ys

addPairs :: Num a => [(a,a)] -> [a]
addPairs = map (\(x,y) -> x + y)

maxPairs :: Ord a => [(a,a)] -> [a]
maxPairs = map (\(x,y) -> max x y)

join :: (Num a, Ord a) => [a] -> [a] -> [a]
join xs ys = maxPairs $ zip lefts rights
  where
    lefts = addPairs $ pairsLeft xs ys
    rights = addPairs $ pairsRight xs ys

pathSums = foldl join []

maxPathSum = maximum . pathSums

main = putStrLn . show . maxPathSum $ triangle
