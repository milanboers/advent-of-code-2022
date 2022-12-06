import Data.List (findIndex, nub)
import Data.Maybe (fromJust)

slidingWindow :: Int -> String -> [String]
slidingWindow n l@(_ : xs) = take n l : slidingWindow n xs
slidingWindow _ [] = []

findAnswer :: Int -> String -> Int
findAnswer n l = n + i
  where
    i = fromJust . findIndex (\w -> nub w == w) $ slidingWindow n l

main :: IO ()
main = do
  line <- getLine
  print $ findAnswer 14 line
