import Data.Char (digitToInt)
import Data.List (nub)
import Data.Matrix (Matrix (ncols, nrows), (!))
import qualified Data.Matrix as Matrix

perimeterAndDeltas :: Matrix Int -> [((Int, Int), (Int, Int))]
perimeterAndDeltas m = left ++ right ++ top ++ bottom
  where
    h = nrows m
    w = ncols m
    left = [((x, 1), (0, 1)) | x <- [1 .. h]]
    right = [((x, h), (0, -1)) | x <- [1 .. h]]
    top = [((1, y), (1, 0)) | y <- [1 .. w]]
    bottom = [((w, y), (-1, 0)) | y <- [1 .. w]]

applyDelta :: (Int, Int) -> (Int, Int) -> (Int, Int)
applyDelta (dx, dy) (x, y) = (x + dx, y + dy)

lineCoords :: Matrix Int -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
lineCoords m c d@(_, 0) = take (nrows m) . iterate (applyDelta d) $ c
lineCoords m c d@(0, _) = take (ncols m) . iterate (applyDelta d) $ c
lineCoords _ _ _ = error "Unsupported arguments"

-- matrix -> start coord -> delta -> coords of trees visible in row
visibleInRow :: Matrix Int -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
visibleInRow m c d = map fst . filter (\(c', hm) -> m ! c' > hm) $ zip lineCoords' maxHeights
  where
    lineCoords' = lineCoords m c d
    line = map (m !) lineCoords'
    maxHeights = scanl max (-1) line

findAnswer :: Matrix Int -> Int
findAnswer m = length visibleTrees
  where
    visibleTrees = nub . concatMap (uncurry (visibleInRow m)) $ perimeterAndDeltas m

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let grid = Matrix.fromLists . (map . map) digitToInt $ input
  print $ findAnswer grid
