import Data.List (find)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import Data.Vector.Generic ((//))
import Text.Read (readMaybe)

type Move = (Int, Int, Int)

type Stack = [Char]

moveSingleCrate :: Vector Stack -> Int -> Int -> Vector Stack
moveSingleCrate stacks from to = stacks // [(from, newFromStack), (to, newToStack)]
  where
    fromStack = stacks ! from
    toStack = stacks ! to
    item = head fromStack
    newFromStack = tail fromStack
    newToStack = item : toStack

doMove :: Vector Stack -> Move -> Vector Stack
doMove stacks (crates, from, to) = iterate moveSingleCrateFromTo stacks !! crates
  where
    moveSingleCrateFromTo stacks' = moveSingleCrate stacks' from to

doMoves :: (Vector Stack, [Move]) -> Vector Stack
doMoves (stacks, moves) = foldl doMove stacks moves

findAnswer :: (Vector Stack, [Move]) -> String
findAnswer = Vector.toList . Vector.map head . doMoves

parseLine :: ([Stack], [Move]) -> String -> ([Stack], [Move])
parseLine (stacks, moves) line | '[' `elem` line = (newStacks, moves)
  where
    stackElements = map (find (/= ']') . filter (/= '[') . filter (/= ' ')) (chunksOf 4 line)
    lengthDiff = length stackElements - length stacks
    stacks' = if lengthDiff > 0 then stacks ++ replicate lengthDiff [] else stacks
    newStacks =
      zipWith
        ( \stack maybeItem ->
            ( if isJust maybeItem
                then stack ++ [fromJust maybeItem]
                else stack
            )
        )
        stacks'
        stackElements
parseLine (stacks, moves) line | 'm' `elem` line = (stacks, moves ++ [newMove])
  where
    toMove [a, b, c] = (a, b - 1, c - 1)
    toMove _ = error "invalid move"
    newMove = toMove . mapMaybe readMaybe . words $ line
parseLine game _ = game

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let (stacks, moves) = foldl parseLine ([], []) input
  print $ findAnswer (Vector.fromList stacks, moves)
