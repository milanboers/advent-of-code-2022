import qualified Data.Bifunctor
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Direction = Left' | Right' | Up | Down deriving (Eq, Show, Ord)

type Dimensions = (Int, Int)

type Gusts = Map (Int, Int) [Direction]

gustToDelta :: Direction -> (Int, Int)
gustToDelta Left' = (-1, 0)
gustToDelta Right' = (1, 0)
gustToDelta Up = (0, -1)
gustToDelta Down = (0, 1)

applyDelta :: (Int, Int) -> (Int, Int) -> (Int, Int)
applyDelta (dx, dy) (x, y) = (dx + x, dy + y)

wrap :: Dimensions -> (Int, Int) -> (Int, Int)
wrap (maxX, maxY) (x, y)
  | x > maxX = (0, y)
  | x < 0 = (maxX, y)
  | y > maxY = (x, 0)
  | y < 0 = (x, maxY)
  | otherwise = (x, y)

moveGust :: Dimensions -> Direction -> (Int, Int) -> (Int, Int)
moveGust dims dir = wrap dims . applyDelta (gustToDelta dir)

moveGusts :: Dimensions -> Gusts -> Gusts
moveGusts dims gusts =
  Map.fromListWith
    (++)
    [(moveGust dims dir pos, [dir]) | (pos, dirs) <- Map.toList gusts, dir <- dirs]

withinDims :: Dimensions -> (Int, Int) -> Bool
withinDims _ (0, -1) = True
withinDims (maxX, maxY) (x, y) | x == maxX && y == maxY + 1 = True
withinDims (maxX, maxY) (x, y) = x >= 0 && x <= maxX && y >= 0 && y <= maxY

possibleMoves :: Dimensions -> Gusts -> (Int, Int) -> Set (Int, Int)
possibleMoves dims gusts (x, y) =
  Set.fromList
    $ filter
      (`Map.notMember` gusts)
    $ filter
      (withinDims dims)
    $ map
      (applyDelta (x, y))
      [(-1, 0), (1, 0), (0, -1), (0, 1), (0, 0)]

minute :: Dimensions -> Gusts -> Set (Int, Int) -> (Gusts, Set (Int, Int))
minute dims gusts poss = (nextGusts, nextPoss)
  where
    nextGusts = moveGusts dims gusts
    nextPoss = Set.unions $ Set.map (possibleMoves dims nextGusts) poss

minutes :: Dimensions -> (Int, Int) -> Gusts -> Set (Int, Int) -> (Int, Gusts)
minutes dims goal gusts poss = if endFound then (1, nextGusts) else (1 + endMinutes, endGusts)
  where
    (nextGusts, nextPoss) = minute dims gusts poss
    endFound = goal `Set.member` nextPoss
    (endMinutes, endGusts) = minutes dims goal nextGusts nextPoss

findAnswer :: Dimensions -> Gusts -> Int
findAnswer dims@(maxX, maxY) gusts = fst pathsMinutes
  where
    startPos = (0, -1)
    goalPos = (maxX, maxY + 1)

    paths = [(startPos, goalPos), (goalPos, startPos), (startPos, goalPos)]
    pathsMinutes =
      foldl
        (\(i, gusts') (start, goal) -> Data.Bifunctor.first (i +) $ minutes dims goal gusts' (Set.singleton start))
        (0, gusts)
        paths

parseGust :: Char -> Direction
parseGust '<' = Left'
parseGust '>' = Right'
parseGust '^' = Up
parseGust 'v' = Down
parseGust _ = error "Invalid gust"

main :: IO ()
main = do
  contents <- getContents
  let input = init . tail $ map (init . tail) (lines contents)
  let gusts = [((x, y), [parseGust v]) | (y, line) <- zip [0 ..] input, (x, v) <- zip [0 ..] line, v /= '.', v /= '#']
  let gustsMap = Map.fromListWith (++) gusts
  let dims = (length (head input) - 1, length input - 1)
  print $ findAnswer dims gustsMap
