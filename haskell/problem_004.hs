import Data.List

digitize :: Int -> [Int]
digitize = unfoldr getDigit
  where
    getDigit x
      | x /= 0 = Just (x `mod` 10, x `div` 10)
      | otherwise = Nothing

isPalindrome :: [Int] -> Bool
isPalindrome x = reverse x == x

main :: IO ()
main = putStrLn . show $ maximum $ [x * y | x <- [999,998..100], y <- [999,998..100], isPalindrome (digitize (x * y))]
