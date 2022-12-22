{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List (transpose)
import Data.List.Split (oneOf, split, splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Facing = Up | Down | Left' | Right' deriving (Eq, Show)

type State = ((Int, Int), Facing)

-- (row no -> (start, end), col no -> (start, end))
type Dimensions = (Map Int (Int, Int), Map Int (Int, Int))

type Blocked = Set (Int, Int)

data TurnDirection = Clockwise | Counterclockwise deriving (Eq, Show)

data Move = Walk Int | Turn TurnDirection deriving (Eq, Show)

blockedCheck :: Blocked -> (Int, Int) -> (Int, Int) -> (Int, Int)
blockedCheck b oldPos newPos = if newPos `Set.notMember` b then newPos else oldPos

step :: Dimensions -> Blocked -> Facing -> (Int, Int) -> (Int, Int)
step (_, dc) b Up pos@(r, c) = blockedCheck b pos newPos
  where
    (colStart, colEnd) = dc Map.! c
    newPos = if r - 1 < colStart then (colEnd, c) else (r - 1, c)
step (_, dc) b Down pos@(r, c) = blockedCheck b pos newPos
  where
    (colStart, colEnd) = dc Map.! c
    newPos = if r + 1 > colEnd then (colStart, c) else (r + 1, c)
step (dr, _) b Left' pos@(r, c) = blockedCheck b pos newPos
  where
    (rowStart, rowEnd) = dr Map.! r
    newPos = if c - 1 < rowStart then (r, rowEnd) else (r, c - 1)
step (dr, _) b Right' pos@(r, c) = blockedCheck b pos newPos
  where
    (rowStart, rowEnd) = dr Map.! r
    newPos = if c + 1 > rowEnd then (r, rowStart) else (r, c + 1)

doMove :: Dimensions -> Blocked -> Move -> State -> State
doMove dims blocked (Walk i) (pos, f) = (newPos, f)
  where
    newPos = (!! i) . iterate (step dims blocked f) $ pos
doMove _ _ (Turn dir) (pos, f) = (pos, turn dir f)
  where
    turn Clockwise Up = Right'
    turn Clockwise Right' = Down
    turn Clockwise Down = Left'
    turn Clockwise Left' = Up
    turn Counterclockwise Up = Left'
    turn Counterclockwise Left' = Down
    turn Counterclockwise Down = Right'
    turn Counterclockwise Right' = Up

doMoves :: Dimensions -> Blocked -> [Move] -> State -> State
doMoves _ _ [] state = state
doMoves d b (m : ms) state = doMoves d b ms newState
  where
    newState = doMove d b m state

facingScore :: Facing -> Int
facingScore Right' = 0
facingScore Down = 1
facingScore Left' = 2
facingScore Up = 3

findAnswer :: Dimensions -> Blocked -> [Move] -> Int
findAnswer d@(rd, _) blocked moves = (r + 1) * 1000 + (c + 1) * 4 + facingScore f
  where
    startPosition = (0, fst $ rd Map.! 0)
    ((r, c), f) = doMoves d blocked moves (startPosition, Right')

prefixWhitespaceLength :: String -> Int
prefixWhitespaceLength = length . takeWhile (== ' ')

rowDimensions :: String -> (Int, Int)
rowDimensions r = (prefixWhitespaceLength r, length r - prefixWhitespaceLength (reverse r) - 1)

rowsDimensions :: [String] -> Map Int (Int, Int)
rowsDimensions m = Map.fromList $ zipWith (\i r -> (i, rowDimensions r)) [0 ..] m

parseDimensions :: [String] -> Dimensions
parseDimensions m = (rowsDimensions m, rowsDimensions $ transpose m)

parseBlocked :: [String] -> Set (Int, Int)
parseBlocked m = Set.fromList [(r, c) | (r, rv) <- zip [0 ..] m, (c, v) <- zip [0 ..] rv, v == '#']

parseMove :: String -> Move
parseMove "R" = Turn Clockwise
parseMove "L" = Turn Counterclockwise
parseMove n = Walk $ read n

parseMoves :: String -> [Move]
parseMoves = map parseMove . (split . oneOf) ['R', 'L']

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let [mapS, movesS] = splitOn [""] input
  let dims = parseDimensions mapS
  let blocked = parseBlocked mapS
  let moves = parseMoves $ head movesS
  print $ findAnswer dims blocked moves
