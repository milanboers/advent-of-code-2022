import Algorithm.Search (bfs)
import Data.List.Split (splitOn)
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as Set

type Cube = (Int, Int, Int)

type Dimensions = (Int, Int, Int, Int, Int, Int)

adjacentPositions :: Cube -> Set Cube
adjacentPositions (x, y, z) =
  Set.fromList
    [ (x + 1, y, z),
      (x - 1, y, z),
      (x, y + 1, z),
      (x, y - 1, z),
      (x, y, z + 1),
      (x, y, z - 1)
    ]

adjacent :: Cube -> Cube -> Bool
adjacent c1 c2 = c1 `Set.member` adjacentPositions c2

adjacentCubes :: Set Cube -> Cube -> Int
adjacentCubes cubes c = Set.size . Set.filter (adjacent c) $ cubes

exposedSides :: Set Cube -> Cube -> Int
exposedSides cubes c = 6 - adjacentCubes cubes c

dimensions :: Set Cube -> Dimensions
dimensions cubes = (Set.findMin xs, Set.findMax xs, Set.findMin ys, Set.findMax ys, Set.findMin zs, Set.findMax zs)
  where
    getX (x, _, _) = x
    getY (_, y, _) = y
    getZ (_, _, z) = z
    xs = Set.map getX cubes
    ys = Set.map getY cubes
    zs = Set.map getZ cubes

outsideDimensions :: Dimensions -> Cube -> Bool
outsideDimensions (minX, maxX, minY, maxY, minZ, maxZ) (x, y, z) =
  x < minX || x > maxX || y < minY || y > maxY || z < minZ || z > maxZ

airs :: Dimensions -> Set Cube -> Set Cube
airs (minX, maxX, minY, maxY, minZ, maxZ) cubes =
  Set.fromList
    [ (x, y, z)
      | x <- [minX .. maxX],
        y <- [minY .. maxY],
        z <- [minZ .. maxZ],
        (x, y, z) `Set.notMember` cubes
    ]

airTrapped :: Dimensions -> Set Cube -> Cube -> Bool
airTrapped dims cubes = isNothing . bfs adjacentNotCube (outsideDimensions dims)
  where
    adjacentNotCube = Set.filter (`Set.notMember` cubes) . adjacentPositions

surfaceArea :: Set Cube -> Int
surfaceArea cubes = sum $ map (exposedSides cubes) (Set.toList cubes)

findAnswer :: Set Cube -> Int
findAnswer cubes = surfaceArea cubes - surfaceArea trappedAirs
  where
    dims = dimensions cubes
    airs' = airs dims cubes
    trappedAirs = Set.filter (airTrapped dims cubes) airs'

tuplify3 :: [Int] -> (Int, Int, Int)
tuplify3 [x, y, z] = (x, y, z)
tuplify3 _ = error "List is not of length 3"

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let cubes = map (tuplify3 . map read . splitOn ",") input
  print $ findAnswer . Set.fromList $ cubes
