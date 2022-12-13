import Data.Char (isDigit)
import Data.List (findIndices, sort)

data Packet = List [Packet] | Single Int deriving (Eq, Show)

instance Ord Packet where
  s@(Single _) `compare` l@(List _) = List [s] `compare` l
  l@(List _) `compare` s@(Single _) = l `compare` List [s]
  (Single x) `compare` (Single y) = x `compare` y
  (List xs) `compare` (List ys) = xs `compare` ys

dividers :: [Packet]
dividers = [List [List [Single 2]], List [List [Single 6]]]

findAnswer :: [Packet] -> Int
findAnswer packets = product . map (+ 1) . findIndices (`elem` dividers) . sort $ packets ++ dividers

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

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let packets = filter (/= "") input
  let packetPairs = map parsePacket packets
  print $ findAnswer packetPairs
