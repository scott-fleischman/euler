import Data.Char
import Data.List
import Text.ParserCombinators.Parsec

quotedName = do
  char '"'
  name <- many (noneOf "\"")
  char '"'
  return name

quotedNames = sepBy quotedName (char ',')

parseNames :: String -> Either ParseError [String]
parseNames input = parse quotedNames "error" input

namesPath = "../data/names.txt"

parseNamesM x = case parseNames x of
  Left e -> []
  Right r -> r

charValue c = ord c - ord 'A' + 1

nameValue = sum . map charValue

pairValue (i, n) = i * n

indexPairs = zip [1..]

nameValueSum = sum . map pairValue . indexPairs . map nameValue

main = do
  namesText <- readFile namesPath
  putStrLn . show . nameValueSum . sort . parseNamesM $ namesText
