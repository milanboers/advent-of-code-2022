import Data.Char (digitToInt)
import Data.Matrix (Matrix (ncols, nrows), (!))
import qualified Data.Matrix as Matrix
import Data.Maybe (isJust)
import Debug.Trace (traceShow)

allDirections :: [(Int, Int)]
allDirections = [(0, 1), (0, -1), (1, 0), (-1, 0)]

applyDelta :: (Int, Int) -> (Int, Int) -> (Int, Int)
applyDelta (dx, dy) (x, y) = (x + dx, y + dy)

validCoord :: Matrix Int -> (Int, Int) -> Bool
validCoord m (row, col) = isJust $ Matrix.safeGet row col m

lineCoords :: Matrix Int -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
lineCoords m c d@(_, 0) = takeWhile (validCoord m) . iterate (applyDelta d) $ c
lineCoords m c d@(0, _) = takeWhile (validCoord m) . iterate (applyDelta d) $ c
lineCoords _ _ _ = error "Unsupported arguments"

takeWhilePlusOne :: (a -> Bool) -> [a] -> [a]
takeWhilePlusOne p xs = case span p xs of
  (prefix, []) -> prefix
  (prefix, x : _) -> prefix ++ [x]

-- matrix -> start coord -> delta -> coords of trees visible in row
visibleInRow :: Matrix Int -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
visibleInRow m c d = takeWhilePlusOne (\c' -> m ! c' < myHeight) viewCoords
  where
    myHeight = m ! c
    viewCoords = tail $ lineCoords m c d

scenicScore :: Matrix Int -> (Int, Int) -> Int
scenicScore m c = product . map length $ visibleTreesInDirs
  where
    visibleTreesInDirs = map (visibleInRow m c) allDirections

findAnswer :: Matrix Int -> Int
findAnswer m = maximum scenicScores
  where
    allCoords = [(x, y) | x <- [1 .. nrows m], y <- [1 .. ncols m]]
    scenicScores = traceShow allCoords $ map (scenicScore m) allCoords

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let grid = Matrix.fromLists . (map . map) digitToInt $ input
  print $ findAnswer grid
