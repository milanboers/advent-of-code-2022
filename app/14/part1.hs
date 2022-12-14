import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

type Dimensions = (Int, Int, Int, Int)

data DropStepResult = Drop (Int, Int) | Rest | Abyss deriving (Eq, Show)

dimensions :: Set (Int, Int) -> Dimensions
dimensions cs = (Set.findMin xs, Set.findMax xs, Set.findMin ys, Set.findMax ys)
  where
    xs = Set.insert 0 $ Set.map fst cs
    ys = Set.insert 0 $ Set.map snd cs

drawLine :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
drawLine (x1, y1) (x2, y2)
  | x1 == x2 || y1 == y2 = [(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]
drawLine _ _ = error "Not a straight line"

pairs :: [a] -> [(a, a)]
pairs [x, y] = [(x, y)]
pairs (x : y : ys) = (x, y) : pairs (y : ys)
pairs _ = error "List must have length >= 2"

drawPath :: [(Int, Int)] -> [(Int, Int)]
drawPath = concatMap (uncurry drawLine) . pairs

dropSandStep :: Dimensions -> Set (Int, Int) -> (Int, Int) -> DropStepResult
dropSandStep (minX, maxX, _, maxY) _ (x, y) | x < minX || x > maxX || y > maxY = Abyss
dropSandStep _ blocked (x, y) | (x, y + 1) `Set.notMember` blocked = Drop (x, y + 1)
dropSandStep _ blocked (x, y) | (x - 1, y + 1) `Set.notMember` blocked = Drop (x - 1, y + 1)
dropSandStep _ blocked (x, y) | (x + 1, y + 1) `Set.notMember` blocked = Drop (x + 1, y + 1)
dropSandStep _ _ _ = Rest

dropSand :: Dimensions -> Set (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
dropSand dims blocked (x, y) = case dropSandStep dims blocked (x, y) of
  Drop (nx, ny) -> dropSand dims blocked (nx, ny)
  Rest -> Just (x, y)
  Abyss -> Nothing

findAnswer' :: Dimensions -> Set (Int, Int) -> Int
findAnswer' dims blocked = case dropSand dims blocked (500, 0) of
  Just (x, y) -> 1 + findAnswer' dims (Set.insert (x, y) blocked)
  Nothing -> 0

findAnswer :: [[(Int, Int)]] -> Int
findAnswer paths = findAnswer' dims allBlocked
  where
    allBlocked = Set.fromList $ concatMap drawPath paths
    dims = dimensions allBlocked

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)
tuplify2 _ = error "List is not of length 2"

parsePath :: String -> [(Int, Int)]
parsePath = map (tuplify2 . map read . splitOn ",") . splitOn " -> "

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let paths = map parsePath input
  print $ findAnswer paths
