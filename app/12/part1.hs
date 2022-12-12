import Algorithm.Search (bfs)
import Data.Matrix (Matrix (nrows), ncols, (!))
import qualified Data.Matrix as Matrix
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import qualified Data.Set as Set

neighbors :: Matrix Int -> (Int, Int) -> Set (Int, Int)
neighbors m (r, c) =
  Set.fromList
    [ (r', c')
      | (r', c') <-
          [ (r - 1, c),
            (r + 1, c),
            (r, c - 1),
            (r, c + 1)
          ],
        isJust $ Matrix.safeGet r' c' m,
        m ! (r', c') <= m ! (r, c) + 1
    ]

nextStates :: Matrix Int -> Set (Int, Int) -> Set (Set (Int, Int))
nextStates m = Set.map (neighbors m)

findAnswer :: Matrix Int -> (Int, Int) -> (Int, Int) -> Int
findAnswer m startPos endPos = length . fromJust $ bfs (nextStates m) (endPos `elem`) (Set.singleton startPos)

elemPos :: Eq a => Matrix a -> a -> (Int, Int)
elemPos m x = head [(r, c) | r <- [1 .. nrows m], c <- [1 .. ncols m], m ! (r, c) == x]

heightMap :: Matrix Char -> Matrix Int
heightMap = fmap height
  where
    height 'S' = height 'a'
    height 'E' = height 'z'
    height x = fromEnum x - fromEnum 'a'

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let rawMatrix = Matrix.fromLists input
  let heightMap' = heightMap rawMatrix
  let startPos = elemPos rawMatrix 'S'
  let endPos = elemPos rawMatrix 'E'
  print $ findAnswer heightMap' startPos endPos
