import Data.Char (isAsciiLower, isAsciiUpper, ord)
import Data.List (intersect)

priority :: Char -> Int
priority x | isAsciiUpper x = ord x - ord 'A' + 27
priority x | isAsciiLower x = ord x - ord 'a' + 1
priority _ = error "Invalid item"

commonItem :: (String, String) -> Char
commonItem (x, y) = head $ intersect x y

findAnswer :: [(String, String)] -> Int
findAnswer = sum . map (priority . commonItem)

parseRucksack :: String -> (String, String)
parseRucksack rs = splitAt compartmentLength rs
  where
    compartmentLength = length rs `div` 2

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let rucksacks = map parseRucksack input
  print $ findAnswer rucksacks
