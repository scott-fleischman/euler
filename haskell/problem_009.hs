targetSum = 1000
minInput = 1
maxInput = 1000

main = putStrLn . show $ [a * b * c | a <- [minInput..maxInput], b <- [a+1..maxInput], let c = floor . sqrt $ fromIntegral (a^2 + b^2), a < b, b < c, a^2 + b^2 == c^2, a + b + c == targetSum]
