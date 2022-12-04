import Data.List.Split (splitOn)

overlaps :: ((Int, Int), (Int, Int)) -> Bool
overlaps ((_, xh), (yl, _)) = yl <= xh

fullyOverlaps :: ((Int, Int), (Int, Int)) -> Bool
fullyOverlaps p@((xl, xh), (yl, yh)) = overlaps p && (xl == yl || yh <= xh)

sortPair :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
sortPair ((xl, xh), (yl, yh)) | xl >= yl = ((yl, yh), (xl, xh))
sortPair p = p

findAnswer :: [((Int, Int), (Int, Int))] -> Int
findAnswer = sum . map (fromEnum . fullyOverlaps . sortPair)

toTuple2 :: [a] -> (a, a)
toTuple2 [x, y] = (x, y)
toTuple2 _ = error "list is not of length 2"

parseRange :: String -> (Int, Int)
parseRange = toTuple2 . map read . splitOn "-"

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine = toTuple2 . map parseRange . splitOn ","

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let pairs = map parseLine input
  print $ findAnswer pairs
