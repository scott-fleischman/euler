sumSquares :: [Int] -> Int
sumSquares = sum . map (^2)

squareSum :: [Int] -> Int
squareSum = (^2) . sum

main :: IO ()
main = putStrLn . show $ squareSum [1..100] - sumSquares [1..100]
