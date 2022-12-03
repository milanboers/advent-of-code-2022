import Data.Char (isAsciiLower, isAsciiUpper, ord)
import Data.List (intersect)
import Data.List.Split (chunksOf)

priority :: Char -> Int
priority x | isAsciiUpper x = ord x - ord 'A' + 27
priority x | isAsciiLower x = ord x - ord 'a' + 1
priority _ = error "Invalid item"

commonItem :: [String] -> Char
commonItem xss = head $ foldl1 intersect xss

findAnswer :: [String] -> Int
findAnswer = sum . map (priority . commonItem) . chunksOf 3

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  print $ findAnswer input
