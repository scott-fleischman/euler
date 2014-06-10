data Spiral = Spiral { diagonals :: [Integer], rest :: [Integer] }
  deriving (Show)

addDiagonal Spiral { diagonals = d, rest = r } n = Spiral { diagonals = hd : d, rest = r' }
  where
    hd = head . drop n $ r
    r' = drop 1 . drop n $ r

addDiagonals s n = foldl addDiagonal s . take 4 $ repeat n

levels n = take ((n - 1) `div` 2) [1,3..]

table n = foldl addDiagonals Spiral { diagonals = [1], rest = [2..] } $ levels n

main = putStrLn . show . sum . diagonals $ table 1001
