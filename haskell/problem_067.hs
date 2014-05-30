import System.Environment
import qualified Data.Text as T

pairsLeft xs ys = zip (xs ++ [0]) ys
pairsRight xs ys = zip (0 : xs) ys

addPairs = map (\(x,y) -> x + y)
maxPairs = map (\(x,y) -> max x y)

join xs ys = maxPairs $ zip lefts rights
  where
    lefts = addPairs $ pairsLeft xs ys
    rights = addPairs $ pairsRight xs ys

pathSums = foldl join []
maxPathSum = maximum . pathSums

makeTriangle :: String -> [[Integer]]
makeTriangle x = map ((map (read . T.unpack)) . T.words) triangleLines
  where
    triangleLines = T.lines $ T.pack x

main = do
  triangleData <- readFile "../data/triangle.txt"
  putStrLn . show . maxPathSum . makeTriangle $ triangleData
