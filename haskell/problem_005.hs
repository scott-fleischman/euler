isEvenlyDivisible :: [Integer] -> Integer -> Bool
isEvenlyDivisible xs x = and $ map ((== 0) . (mod x)) xs

main :: IO ()
main = putStrLn . show $ take 1 (filter (isEvenlyDivisible [1..20]) [1..])
