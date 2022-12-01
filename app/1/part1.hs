import Data.List.Split (splitOn)

findAnswer :: [[Int]] -> Int
findAnswer = maximum . map sum

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let elvesStr = splitOn [""] input
  let elves = map (map read) elvesStr
  print $ findAnswer elves
