import Data.List (sort)
import Data.List.Split (splitOn)

type Cube = (Int, Int, Int)

adjacent :: Cube -> Cube -> Bool
adjacent (x1, y1, z1) (x2, y2, z2) = sort [abs (x1 - x2), abs (y1 - y2), abs (z1 - z2)] == [0, 0, 1]

adjacentCubes :: [Cube] -> Cube -> Int
adjacentCubes cubes c = length . filter (adjacent c) $ cubes

exposedSides :: [Cube] -> Cube -> Int
exposedSides cubes c = 6 - adjacentCubes cubes c

findAnswer :: [Cube] -> Int
findAnswer cubes = sum $ map (exposedSides cubes) cubes

tuplify3 :: [Int] -> (Int, Int, Int)
tuplify3 [x, y, z] = (x, y, z)
tuplify3 _ = error "List is not of length 3"

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let cubes = map (tuplify3 . map read . splitOn ",") input
  print $ findAnswer cubes
