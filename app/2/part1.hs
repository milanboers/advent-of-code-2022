data Move = R | P | S deriving (Eq, Show)

shapeScore :: Move -> Int
shapeScore R = 1
shapeScore P = 2
shapeScore S = 3

winScore :: (Move, Move) -> Int
winScore (S, R) = 6
winScore (R, P) = 6
winScore (P, S) = 6
winScore (x, y) | x == y = 3
winScore _ = 0

gameScore :: (Move, Move) -> Int
gameScore (x, y) = winScore (x, y) + shapeScore y

findAnswer :: [(Move, Move)] -> Int
findAnswer = sum . map gameScore

parseOpponentMove :: Char -> Move
parseOpponentMove 'A' = R
parseOpponentMove 'B' = P
parseOpponentMove 'C' = S
parseOpponentMove _ = error "Invalid opponenent move"

parseOwnMove :: Char -> Move
parseOwnMove 'X' = R
parseOwnMove 'Y' = P
parseOwnMove 'Z' = S
parseOwnMove _ = error "Invalid own move"

parseGame :: String -> (Move, Move)
parseGame (x : _ : y : "") = (parseOpponentMove x, parseOwnMove y)
parseGame _ = error "Invalid game line"

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let games = map parseGame input
  print $ findAnswer games
