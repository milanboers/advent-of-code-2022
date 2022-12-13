import Data.Char (isDigit)
import Data.List (findIndices)
import Data.List.Split (splitOn)

data Packet = List [Packet] | Single Int deriving (Eq, Show)

instance Ord Packet where
  s@(Single _) `compare` l@(List _) = List [s] `compare` l
  l@(List _) `compare` s@(Single _) = l `compare` List [s]
  (Single x) `compare` (Single y) = x `compare` y
  (List xs) `compare` (List ys) = xs `compare` ys

findAnswer :: [(Packet, Packet)] -> Int
findAnswer = sum . map (+ 1) . findIndices (uncurry (<))

parsePacket :: String -> Packet
parsePacket = fst . parsePacket'
  where
    parsePacket' ('[' : xs) = (List pxs, rr)
      where
        (pxs, rr) = parseList xs

        parseList "" = ([], "")
        parseList (']' : xs') = ([], xs')
        parseList xs' = (firstPacket : otherPackets, r'')
          where
            (firstPacket, r) = parsePacket' xs'
            (otherPackets, r'') = parseList r
    parsePacket' (',' : xs) = parsePacket' xs
    parsePacket' x = (Single $ read n, r)
      where
        (n, r) = span isDigit x

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)
tuplify2 _ = error "List is not of length 2"

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let pairs = splitOn [""] input
  let packetPairs = map (tuplify2 . map parsePacket) pairs
  print $ findAnswer packetPairs
