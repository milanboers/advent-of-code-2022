import Data.List (find, uncons)
import Data.Maybe (fromJust, mapMaybe)

snafuDecMap :: [(Char, Int)]
snafuDecMap = [('=', -2), ('-', -1), ('0', 0), ('1', 1), ('2', 2)]

snafuToDec :: Char -> Int
snafuToDec c = snd . fromJust . find ((== c) . fst) $ snafuDecMap

decToSnafu :: Int -> Char
decToSnafu d = fst . fromJust . find ((== d) . snd) $ snafuDecMap

snafuSum :: [String] -> String
snafuSum xs = snafuSum' (map reverse xs) 0
  where
    snafuSum' [] 0 = []
    snafuSum' xs' acc = snafuSum' tails d ++ [lastSnafuDigit]
      where
        unconss = mapMaybe uncons xs'
        heads = map fst unconss
        tails = map snd unconss
        decSum = acc + sum (map snafuToDec heads)
        (d, m) = (decSum + 2) `divMod` 5
        lastSnafuDigit = decToSnafu (m - 2)

stripZeros :: String -> String
stripZeros ('0' : xs) = xs
stripZeros xs = xs

findAnswer :: [String] -> String
findAnswer = stripZeros . snafuSum

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  print $ findAnswer input
