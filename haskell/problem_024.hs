import Data.List

permDigits = permutations $ [0..9]
perms = map (foldl (\a v -> a * 10 + v) 0) permDigits
lexPerms = sort perms

main = putStrLn . show $ lexPerms !! 999999
