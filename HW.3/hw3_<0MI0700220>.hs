import Data.List (sort, groupBy, partition)
import Data.Char (toLower)

type Command = String
type Size = Int
type Name = String

data FileSystem = Directory Name [FileSystem] | File Name Size
    deriving (Eq, Show)

generateFileSystem :: [Command] -> FileSystem
generateFileSystem commands = sortFileSystem $ interpretCommands commands []

interpretCommands :: [Command] -> [FileSystem] -> [FileSystem]
interpretCommands [] fs = fs
interpretCommands (cmd:cmds) fs
    | "$ cd" `isPrefixOf` cmd = interpretCommands cmds fs
    | "dir" `isPrefixOf` cmd = let dirName = drop 4 cmd
                                    (subCmds, remainingCmds) = splitCommands cmds
                                    subFs = generateSubFileSystem subCmds
                                in interpretCommands remainingCmds (Directory dirName subFs : fs)
    | otherwise = let (fileName, size) = parseFileInfo cmd
                  in interpretCommands cmds (File fileName size : fs)

splitCommands :: [Command] -> ([Command], [Command])
splitCommands = span (not . isCommand)

isCommand :: Command -> Bool
isCommand ('$':_) = True
isCommand _ = False

generateSubFileSystem :: [Command] -> [FileSystem]
generateSubFileSystem = interpretCommands

parseFileInfo :: Command -> (Name, Size)
parseFileInfo cmd = let (name, sizeStr) = span (/= ' ') cmd
                        size = read sizeStr
                    in (name, size)

sortFileSystem :: [FileSystem] -> FileSystem
sortFileSystem fs = Directory "/" (sortDirectoryContents fs)

sortDirectoryContents :: [FileSystem] -> [FileSystem]
sortDirectoryContents fs =
    let (dirs, files) = partitionDirectoriesAndFiles fs
        sortedDirs = sortByName dirs
        sortedFiles = sortByName files
    in sortedDirs ++ sortedFiles

partitionDirectoriesAndFiles :: [FileSystem] -> ([FileSystem], [FileSystem])
partitionDirectoriesAndFiles = partition isDirectory
    where
        isDirectory (Directory _ _) = True
        isDirectory _ = False

sortByName :: [FileSystem] -> [FileSystem]
sortByName = sortBy (\a b -> compare (lowercaseName a) (lowercaseName b))
    where
        lowercaseName (Directory name _) = map toLower name
        lowercaseName (File name _) = map toLower name

commands :: [Command]
commands = ["$ cd /","$ ls","dir a","14848514 b.txt","8504156 c.dat","dir d","$ cd a","$ ls","dir e","29116 f","2557 g","62596 h.lst","$ cd e","$ ls","584 i","$ cd ..","$ cd ..","$ cd d","$ ls","4060174 j","8033020 d.log","5626152 d.ext","7214296 k"]


main :: IO ()
main = do
  print $ generateFileSystem commands -- == Directory "/" [Directory "a" [Directory "e" [File "i" 584], File "f" 29116, File "g" 2557, File "h.lst" 62596], Directory "d" [File "d.ext" 5626152, File "d.log" 8033020, File "j" 4060174, File "k" 7214296], File "b.txt" 14848514, File "c.dat" 8504156]
  {-print $ getParentSize (generateFileSystem commands) "i" == 584
  print $ getParentSize (generateFileSystem commands) "g" == 94853
  print $ getParentSize (generateFileSystem commands) "b.txt" == 48381165
  print $ getParentSize (generateFileSystem commands) "abc" == -1 -}


  