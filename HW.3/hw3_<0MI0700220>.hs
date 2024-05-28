import Data.List (sortBy, span)
import Data.Char (toLower)
import Data.Function (on)

type Command = String
type Size = Int
type Name = String

data FileSystem = Directory Name [FileSystem] | File Name Size deriving (Eq, Show)

generateFileSystem :: [Command] -> FileSystem
generateFileSystem commands = buildTree $ parseCommands [] commands

-- Parse commands into a list of filesystems
parseCommands :: [Name] -> [Command] -> [FileSystem]
parseCommands _ [] = []
parseCommands path (c:cs)
    | "$ cd " `elem` words c = 
        let dir = drop 5 c
            newPath = if dir == ".." then init path else path ++ [dir]
        in parseCommands newPath cs
    | "$ ls" `elem` words c = 
        let (entries, rest) = span (\cmd -> head cmd /= '$') cs
            parsedEntries = map (parseEntry path) entries
        in parsedEntries ++ parseCommands path rest
    | otherwise = parseCommands path cs

-- Parse a single entry (file or directory)
parseEntry :: [Name] -> Command -> FileSystem
parseEntry path cmd
    | "dir " `elem` words cmd = 
        let dirName = drop 4 cmd
        in Directory (last path ++ "/" ++ dirName) []
    | otherwise = 
        let (sizeStr:fileName:_) = words cmd
            size = read sizeStr
        in File fileName size

-- Build the tree from parsed entries
buildTree :: [FileSystem] -> FileSystem
buildTree entries = 
    let groupedEntries = groupEntries entries
        sortedEntries = map (\(dir, files) -> Directory dir (sortTree files)) groupedEntries
    in Directory "/" (sortTree sortedEntries)

-- Group entries by directory
groupEntries :: [FileSystem] -> [(Name, [FileSystem])]
groupEntries = foldr groupByDir []
  where
    groupByDir (File name size) acc = case acc of
        [] -> [("", [File name size])]
        ((dir, files):rest) -> (dir, File name size:files):rest
    groupByDir (Directory name files) acc = (name, files) : acc

-- Sort directories and files: directories first, then files, both case-insensitively
sortTree :: [FileSystem] -> [FileSystem]
sortTree = sortBy compareFS
  where
    lowerName (Directory name _) = map toLower name
    lowerName (File name _)      = map toLower name
    compareFS a b = case (a, b) of
        (Directory{}, File{}) -> LT
        (File{}, Directory{}) -> GT
        (Directory n1 _, Directory n2 _) -> compare (map toLower n1) (map toLower n2)
        (File n1 _, File n2 _) -> compare (map toLower n1) (map toLower n2)


-- ///////////////////////////////////////////////////////////// --


getParentSize :: FileSystem -> Name -> Size
getParentSize fs filename = findSize fs
  where
    findSize :: FileSystem -> Size
    findSize (Directory _ children)
      | any (fileExists filename) children = sum (map totalSize children)
      | otherwise = let sizes = filter (/= -1) (map findSize children)
                    in if null sizes then -1 else minimum sizes
    findSize _ = -1

    fileExists :: Name -> FileSystem -> Bool
    fileExists name (File n _) = n == name
    fileExists name (Directory _ children) = any (fileExists name) children

    totalSize :: FileSystem -> Size
    totalSize (File _ size) = size
    totalSize (Directory _ children) = sum (map totalSize children)



commands :: [Command]
commands = ["$ cd /","$ ls","dir a","14848514 b.txt","8504156 c.dat","dir d","$ cd a","$ ls","dir e","29116 f","2557 g","62596 h.lst","$ cd e","$ ls","584 i","$ cd ..","$ cd ..","$ cd d","$ ls","4060174 j","8033020 d.log","5626152 d.ext","7214296 k"]


main :: IO ()
main = do
  print $ generateFileSystem commands -- == Directory "/" [Directory "a" [Directory "e" [File "i" 584], File "f" 29116, File "g" 2557, File "h.lst" 62596], Directory "d" [File "d.ext" 5626152, File "d.log" 8033020, File "j" 4060174, File "k" 7214296], File "b.txt" 14848514, File "c.dat" 8504156]
  print $ getParentSize (generateFileSystem commands) "i" == 584
  print $ getParentSize (generateFileSystem commands) "g" == 94853
  print $ getParentSize (generateFileSystem commands) "b.txt" == 48381165
  print $ getParentSize (generateFileSystem commands) "abc" == -1 


  