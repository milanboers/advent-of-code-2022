import Data.List.Split (dropDelims, dropInitBlank, onSublist, splitOn)
import Data.List.Split.Internals (split)

-- filesystem
data Entry = Dir String [Entry] | File String Int deriving (Eq, Show)

-- commands
data LsOutputLine = LsDirOutput String | LsFileOutput (String, Int) deriving (Eq, Show)

data Command = Cd String | Ls [LsOutputLine] deriving (Eq, Show)

totalSize :: Entry -> Int
totalSize (Dir _ entries) = sum . map totalSize $ entries
totalSize (File _ size) = size

allDirs :: Entry -> [Entry]
allDirs d@(Dir _ entries) = d : concatMap allDirs entries
allDirs (File _ _) = []

findAnswer :: Entry -> Int
findAnswer = sum . filter (< 100000) . map totalSize . allDirs

isDir :: Entry -> String -> Bool
isDir (Dir x _) y = x == y
isDir _ _ = False

entryName :: Entry -> String
entryName (Dir x _) = x
entryName (File x _) = x

lsOutputLineToNewEntry :: LsOutputLine -> Entry
lsOutputLineToNewEntry (LsFileOutput (name, size)) = File name size
lsOutputLineToNewEntry (LsDirOutput name) = Dir name []

-- dir -> path of new entry -> new entry -> new dir
addToFs :: Entry -> [String] -> Entry -> Entry
addToFs (Dir dirName entries) (p : ps) newEntry = Dir dirName newEntries
  where
    newEntries = map (\entry -> if isDir entry p then addToFs entry ps newEntry else entry) entries
addToFs (Dir dirName entries) [] newEntry = Dir dirName newEntries
  where
    alreadyExists = elem (entryName newEntry) . map entryName $ entries
    -- don't replace existing items
    newEntries = if alreadyExists then entries else newEntry : entries
addToFs (File _ _) _ _ = error "must add to directory"

-- (root, path) -> command -> (new root, new path)
doCommand :: (Entry, [String]) -> Command -> (Entry, [String])
doCommand (root, _) (Cd "/") = (root, [])
doCommand (root, path) (Cd "..") = (root, init path)
doCommand (root, path) (Cd x) = (root, path ++ [x])
doCommand (root, path) (Ls lsOutputs) = (newRoot, path)
  where
    newEntries = map lsOutputLineToNewEntry lsOutputs
    newRoot = foldl (`addToFs` path) root newEntries

commandsToFs :: [Command] -> Entry
commandsToFs = fst . foldl (\(e, path) cmd -> doCommand (e, path) cmd) (initialRoot, [])
  where
    initialRoot = Dir "/" []

parseLsOutputLine :: String -> LsOutputLine
parseLsOutputLine ('d' : 'i' : 'r' : ' ' : x) = LsDirOutput x
parseLsOutputLine xs = LsFileOutput (parseSplitLsOutputLine . splitOn " " $ xs)
  where
    parseSplitLsOutputLine [a, b] = (b, read a :: Int)
    parseSplitLsOutputLine _ = error "Invalid ls output line"

parseCommand :: [String] -> Command
parseCommand ['c' : 'd' : ' ' : x] = Cd x
parseCommand ("ls" : xs) = Ls (map parseLsOutputLine xs)
parseCommand _ = error "Invalid command"

main :: IO ()
main = do
  contents <- getContents
  let commandsStr = (split . dropInitBlank . dropDelims . onSublist) "$ " contents
  let commands = map (parseCommand . lines) commandsStr
  let fs = commandsToFs commands
  print $ findAnswer fs
