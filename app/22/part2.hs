{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List (find)
import Data.List.Split (oneOf, split, splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Facing = Up | Down | Left' | Right' deriving (Eq, Show, Ord)

type State = ((Int, Int), Facing)

type Blocked = Set (Int, Int)

data TurnDirection = Clockwise | Counterclockwise deriving (Eq, Show)

data Move = Walk Int | Turn TurnDirection deriving (Eq, Show)

-- Input specific stuff
n :: Int
n = 50

sideLocation :: Int -> (Int, Int)
sideLocation 1 = (0, n)
sideLocation 2 = (0, n * 2)
sideLocation 3 = (n, n)
sideLocation 4 = (2 * n, 0)
sideLocation 5 = (2 * n, n)
sideLocation 6 = (3 * n, 0)
sideLocation _ = error "Invalid side"

-- (old side, old facing) -> (new side, new facing, flipped)
mappings :: Map (Int, Facing) (Int, Facing, Bool)
mappings =
  Map.fromList
    [ ((1, Up), (6, Right', False)),
      ((6, Left'), (1, Down, False)),
      ((2, Up), (6, Up, False)),
      ((6, Down), (2, Down, False)),
      ((4, Up), (3, Right', False)),
      ((3, Left'), (4, Down, False)),
      ((3, Right'), (2, Up, False)),
      ((2, Down), (3, Left', False)),
      ((5, Down), (6, Left', False)),
      ((6, Right'), (5, Up, False)),
      ((1, Left'), (4, Right', True)),
      ((4, Left'), (1, Right', True)),
      ((2, Right'), (5, Left', True)),
      ((5, Right'), (2, Left', True))
    ]

-- End input specific stuff

flipN :: Int -> Int
flipN i = (n - 1) - i

normalize :: Int -> (Int, Int) -> (Int, Int)
normalize side (r, c) = (r - sr, c - sc)
  where
    (sr, sc) = sideLocation side

onPerimiter :: Int -> State -> Bool
onPerimiter side state = case state of
  ((r, c), Up) -> c >= c' && c < c' + n && r == r' - 1
  ((r, c), Down) -> c >= c' && c < c' + n && r == r' + n
  ((r, c), Left') -> r >= r' && r < r' + n && c == c' - 1
  ((r, c), Right') -> r >= r' && r < r' + n && c == c' + n
  where
    (r', c') = sideLocation side

fromSideD :: Facing -> (Int, Int) -> Int
fromSideD Down (_, c) = c
fromSideD Up (_, c) = c
fromSideD Left' (r, _) = r
fromSideD Right' (r, _) = r

toSideD :: Facing -> Int -> (Int, Int)
toSideD Down x = (0, x)
toSideD Up x = (n - 1, x)
toSideD Left' x = (x, n - 1)
toSideD Right' x = (x, 0)

mapSide' :: State -> Int -> Int -> Facing -> Bool -> State
mapSide' (p, oldFacing) oldSide newSide newFacing flipped = (applyDelta sl (r, c), newFacing)
  where
    normalizedPos = normalize oldSide p
    x = fromSideD oldFacing normalizedPos
    x' = if flipped then flipN x else x
    (r, c) = toSideD newFacing x'
    sl = sideLocation newSide

mapSide :: State -> State
mapSide s@(_, fromFacing) = case sideMapping of
  Just (fromSide, toSide, toFacing, flipped) -> mapSide' s fromSide toSide toFacing flipped
  Nothing -> s
  where
    sideMapping = do
      fromSide <- find (`onPerimiter` s) [1 .. 6]
      (toSide, toFacing, flipped) <- Map.lookup (fromSide, fromFacing) mappings
      return (fromSide, toSide, toFacing, flipped)

blockedCheck :: Blocked -> State -> State -> State
blockedCheck b oldState newState@(newPos, _) = if newPos `Set.notMember` b then newState else oldState

facingToDelta :: Facing -> (Int, Int)
facingToDelta Up = (-1, 0)
facingToDelta Down = (1, 0)
facingToDelta Left' = (0, -1)
facingToDelta Right' = (0, 1)

applyDelta :: (Int, Int) -> (Int, Int) -> (Int, Int)
applyDelta (dr, dc) (r, c) = (dr + r, dc + c)

step :: Blocked -> State -> State
step b state@(pos, f) = blockedCheck b state newState
  where
    newPos = applyDelta (facingToDelta f) pos
    newState = mapSide (newPos, f)

doMove :: Blocked -> Move -> State -> State
doMove blocked (Walk i) state = newState
  where
    newState = (!! i) . iterate (step blocked) $ state
doMove _ (Turn dir) (pos, f) = (pos, turn dir f)
  where
    turn Clockwise Up = Right'
    turn Clockwise Right' = Down
    turn Clockwise Down = Left'
    turn Clockwise Left' = Up
    turn Counterclockwise Up = Left'
    turn Counterclockwise Left' = Down
    turn Counterclockwise Down = Right'
    turn Counterclockwise Right' = Up

doMoves :: Blocked -> [Move] -> State -> State
doMoves _ [] state = state
doMoves b (m : ms) state = doMoves b ms newState
  where
    newState = doMove b m state

facingScore :: Facing -> Int
facingScore Right' = 0
facingScore Down = 1
facingScore Left' = 2
facingScore Up = 3

findAnswer :: Blocked -> [Move] -> Int
findAnswer blocked moves = (r + 1) * 1000 + (c + 1) * 4 + facingScore f
  where
    startPosition = (0, n)
    ((r, c), f) = doMoves blocked moves (startPosition, Right')

parseBlocked :: [String] -> Set (Int, Int)
parseBlocked m = Set.fromList [(r, c) | (r, rv) <- zip [0 ..] m, (c, v) <- zip [0 ..] rv, v == '#']

parseMove :: String -> Move
parseMove "R" = Turn Clockwise
parseMove "L" = Turn Counterclockwise
parseMove n' = Walk $ read n'

parseMoves :: String -> [Move]
parseMoves = map parseMove . (split . oneOf) ['R', 'L']

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let [mapS, movesS] = splitOn [""] input
  let blocked = parseBlocked mapS
  let moves = parseMoves $ head movesS
  print $ findAnswer blocked moves
