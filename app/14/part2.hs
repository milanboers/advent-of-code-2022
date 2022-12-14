import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

data DropStepResult = Drop (Int, Int) | Rest deriving (Eq, Show)

maxYf :: Set (Int, Int) -> Int
maxYf = (+ 2) . Set.findMax . Set.insert 0 . Set.map snd

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

canDrop :: Int -> Set (Int, Int) -> (Int, Int) -> Bool
canDrop maxY blocked (x, y) | (x, y) `Set.notMember` blocked && y < maxY = True
canDrop _ _ _ = False

dropSandStep :: Int -> Set (Int, Int) -> (Int, Int) -> DropStepResult
dropSandStep maxY blocked (x, y) | canDrop maxY blocked (x, y + 1) = Drop (x, y + 1)
dropSandStep maxY blocked (x, y) | canDrop maxY blocked (x - 1, y + 1) = Drop (x - 1, y + 1)
dropSandStep maxY blocked (x, y) | canDrop maxY blocked (x + 1, y + 1) = Drop (x + 1, y + 1)
dropSandStep _ _ _ = Rest

dropSand :: Int -> Set (Int, Int) -> (Int, Int) -> (Int, Int)
dropSand maxY blocked (x, y) = case dropSandStep maxY blocked (x, y) of
  Drop (nx, ny) -> dropSand maxY blocked (nx, ny)
  Rest -> (x, y)

findAnswer' :: Int -> Set (Int, Int) -> Int
findAnswer' maxY blocked = case dropSand maxY blocked (500, 0) of
  (500, 0) -> 1
  (x, y) -> 1 + findAnswer' maxY (Set.insert (x, y) blocked)

findAnswer :: [[(Int, Int)]] -> Int
findAnswer paths = findAnswer' maxY allBlocked
  where
    allBlocked = Set.fromList $ concatMap drawPath paths
    maxY = maxYf allBlocked

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
