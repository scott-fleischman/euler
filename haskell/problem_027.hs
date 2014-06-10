import qualified Data.Set as Set
import Data.List

isOddPrime x = all (/= 0) . map (mod x) . takeWhile (\n -> n * n <= x) $ [3,5..]
primes = 2 : [x | x <- [3,5..], isOddPrime x]

primeLimit = 1000

primeSet :: Set.Set Int
primeSet = Set.fromList . take primeLimit $ primes

primeCount f = length . takeWhile (\x -> Set.member x primeSet) . map f $ [0..]

coefficients = [-1000..1000]

quadratic a b n = n^2 + a * n + b

quadPrimes = [(a,b,p) | a <- coefficients, b <- coefficients, let p = primeCount (quadratic a b)]

maxQuadPrime = maximumBy (\(_,_,p1) (_,_,p2)-> compare p1 p2) quadPrimes

coeffProduct (a, b, c) = a * b

main = putStrLn . show . coeffProduct $ maxQuadPrime
