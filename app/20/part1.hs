import Data.Maybe (fromJust)
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector

-- (index in original list, x)
type Number = (Int, Int)

type State = Vector Number

moveX :: State -> Number -> Int -> State
moveX v n x = newV
  where
    currentPosition = fromJust $ Vector.elemIndex n v

    before = Vector.slice 0 currentPosition v
    after = Vector.slice (currentPosition + 1) (Vector.length v - currentPosition - 1) v

    restVector = before Vector.++ after

    newPosition = (currentPosition + x) `mod` Vector.length restVector

    before' = Vector.slice 0 newPosition restVector
    after' = Vector.slice newPosition (Vector.length restVector - newPosition) restVector

    newV = Vector.concat [before', Vector.singleton n, after']

findValueAtIndex :: State -> Int -> Int
findValueAtIndex v i = snd $ v ! (i `mod` Vector.length v)

findAnswer :: State -> Int
findAnswer s = sum $ map (\x -> findValueAtIndex movedState (zeroPosition + x)) [1000, 2000, 3000]
  where
    movedState = foldl (\s' n@(_, nv) -> moveX s' n nv) s s
    zeroPosition = fromJust $ Vector.findIndex (\(_, x) -> x == 0) movedState

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let numbers = zip [0 ..] $ map read input
  let v = Vector.fromList numbers
  print $ findAnswer v
