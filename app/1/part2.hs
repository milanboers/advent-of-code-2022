import Data.List (sort)
import Data.List.Split (splitOn)

findAnswer :: [[Int]] -> Int
findAnswer = sum . take 3 . reverse . sort . map sum

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let elvesStr = splitOn [""] input
  let elves = map (map read) elvesStr
  print $ findAnswer elves
