import Data.List (sortOn)
import Data.Char (toLower)

data FileSystem = Directory Name [FileSystem] | File Name Size
  deriving (Eq, Show)

type Command = String
type Size = Int
type Name = String

-- Sort directories and files
sortFileSystem :: [FileSystem] -> [FileSystem]
sortFileSystem = sortOn getNameCaseInsensitive . sortOn isDirectory
  where
    getNameCaseInsensitive fs = map toLower (getName fs)

isDirectory :: FileSystem -> Bool
isDirectory (Directory _ _) = True
isDirectory _ = False

getName :: FileSystem -> Name
getName (Directory name _) = name
getName (File name _) = name

-- Generate FileSystem from commands
generateFileSystem :: [Command] -> FileSystem
generateFileSystem commands = buildFileSystem (Directory "/" []) commands ["/"]

-- Build FileSystem recursively
buildFileSystem :: FileSystem -> [Command] -> [Name] -> FileSystem
buildFileSystem fs [] _ = fs
buildFileSystem fs (cmd:cmds) path
  | cmd == "$ cd /" = buildFileSystem fs cmds ["/"]
  | cmd == "$ cd .." = buildFileSystem fs cmds (init path)
  | take 4 cmd == "$ cd" = buildFileSystem fs cmds (path ++ [drop 5 cmd])
  | cmd == "$ ls" = let (entries, rest) = span (not . isCommand) cmds
                    in buildFileSystem (addEntries fs path entries) rest path
  | otherwise = buildFileSystem fs cmds path

-- Check if a command is a file/directory entry
isCommand :: Command -> Bool
isCommand ('$':_) = True
isCommand _       = False

-- Add entries to the current directory in the FileSystem
addEntries :: FileSystem -> [Name] -> [Command] -> FileSystem
addEntries fs path entries = foldl (addEntry path) fs entries

-- Add a single entry to the specified path in the FileSystem
addEntry :: [Name] -> FileSystem -> Command -> FileSystem
addEntry [] fs _ = fs
addEntry ["/"] (Directory name contents) entry = Directory name (sortFileSystem (parseEntry entry : contents))
addEntry (p:ps) (Directory name contents) entry =
  Directory name (map update contents)
  where
    update dir@(Directory dName _)
      | dName == p = addEntry ps dir entry
      | otherwise  = dir
    update file = file
addEntry _ fs _ = fs

-- Parse a single entry command
parseEntry :: Command -> FileSystem
parseEntry entry
  | take 3 entry == "dir" = Directory (drop 4 entry) []
  | otherwise = let (size, name) = span (/= ' ') entry
                in File (drop 1 name) (read size)

-- Get the size of the smallest directory containing the file
getParentSize :: FileSystem -> Name -> Size
getParentSize fs filename = minimumSize (findContainingDirs fs filename)

-- Find all directories containing the specified file
findContainingDirs :: FileSystem -> Name -> [Size]
findContainingDirs (File _ _) _ = []
findContainingDirs (Directory _ contents) filename =
  let subdirSizes = concatMap (`findContainingDirs` filename) contents
  in if any (`containsFile` filename) contents
     then directorySize (Directory "" contents) : subdirSizes
     else subdirSizes

-- Check if a FileSystem contains the file
containsFile :: FileSystem -> Name -> Bool
containsFile (File name _) target = name == target
containsFile (Directory _ contents) target = any (`containsFile` target) contents

-- Calculate the total size of a directory
directorySize :: FileSystem -> Size
directorySize (File _ size) = size
directorySize (Directory _ contents) = sum (map directorySize contents)

-- Get the minimum size from a list, or -1 if the list is empty
minimumSize :: [Size] -> Size
minimumSize [] = -1
minimumSize sizes = minimum sizes

commands :: [Command]
commands = ["$ cd /","$ ls","dir a","14848514 b.txt","8504156 c.dat","dir d","$ cd a","$ ls","dir e","29116 f","2557 g","62596 h.lst","$ cd e","$ ls","584 i","$ cd ..","$ cd ..","$ cd d","$ ls","4060174 j","8033020 d.log","5626152 d.ext","7214296 k"]

main :: IO ()
main = do
  let fs = generateFileSystem commands
  print fs
  print $ getParentSize fs "i" == 584      -- Should output 584
  print $ getParentSize fs "g" == 94853    -- Should output 94853
  print $ getParentSize fs "b.txt" == 48381165  -- Should output 48381165
  print $ getParentSize fs "abc" == -1     -- Should output -1
