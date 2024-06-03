import Data.List
import Data.Char 

type Command = String
type Size = Int
type Name = String

data FileSystem = Directory Name [FileSystem] | File Name Size 
 deriving (Eq, Show)

generateFileSystem :: [Command] -> FileSystem
generateFileSystem (cmd:cmds) = fst $ helper (Directory "/" []) ["/"] cmds
 where
    helper :: FileSystem -> [String] -> [Command] -> (FileSystem, [Command])
    helper fs path [] = (fs, [])
    helper fs path (cmd:cmds)
      | isPrefixOf "$ cd " cmd = helper fs (updatePath path (drop 5 cmd)) cmds
      | isPrefixOf "$ ls" cmd = helper fs path cmds
      | isPrefixOf "dir " cmd = helper (addDir fs path dirName) path cmds
      | otherwise = helper (addFile fs path fileName fileSize) path cmds
      where
        dirName = drop 4 cmd
        (fileSize, fileName) = let (sizeStr:nameParts) = words cmd in (read sizeStr, unwords nameParts)
    

    addDir :: FileSystem -> [String] -> Name -> FileSystem
    addDir (Directory name contents) ["/"] newDir = Directory name (sortFileSystem (Directory newDir [] : contents))
    addDir (Directory name contents) (p:ps) newDir
      | name == p = Directory name (sortFileSystem (map (\c -> addDir c ps newDir) contents ++ if null ps then [Directory newDir []] else []))
      | otherwise = Directory name contents
    addDir fs _ _ = fs
    

    addFile :: FileSystem -> [String] -> Name -> Size -> FileSystem
    addFile (Directory name contents) ["/"] newFile size = Directory name (sortFileSystem (File newFile size : contents))
    addFile (Directory name contents) (p:ps) newFile size
      | name == p = Directory name (sortFileSystem (map (\c -> addFile c ps newFile size) contents ++ if null ps then [File newFile size] else []))
      | otherwise = Directory name contents
    addFile fs _ _ _ = fs

updatePath :: [String] -> String -> [String]
updatePath _ "/" = ["/"]
updatePath path ".." = if length path > 1 then init path else path
updatePath path dir = path ++ [dir]

sortFileSystem :: [FileSystem] -> [FileSystem]
sortFileSystem = sortOn (\fs -> (isFile fs, map toLower (getName fs)))
 where
  isFile (File _ _) = True
  isFile _ = False
  getName (Directory name _) = name
  getName (File name _) = name


-- ///////////////////////////////////////////////////////////// --

directorySize :: FileSystem -> Size
directorySize (File _ size) = size
directorySize (Directory _ contents) = sum (map directorySize contents)

getParentSize :: FileSystem -> Name -> Size
getParentSize fs name = getSize fs
  where
    getSize (File _ _) = -1
    getSize dir@(Directory _ contents) =
      let sizes = map getSize contents
          fileInDir = any (hasFile name) contents
      in if fileInDir
         then minimum (directorySize dir : filter (/= -1) sizes)
         else -1

    hasFile name (File fname _) = name == fname
    hasFile name (Directory _ contents) = any (hasFile name) contents

commands :: [Command]
commands = ["$ cd /","$ ls","dir a","14848514 b.txt","8504156 c.dat","dir d","$ cd a","$ ls","dir e","29116 f","2557 g","62596 h.lst","$ cd e","$ ls","584 i","$ cd ..","$ cd ..","$ cd d","$ ls","4060174 j","8033020 d.log","5626152 d.ext","7214296 k"]

main :: IO ()
main = do
  print $ generateFileSystem commands == Directory "/" [Directory "a" [Directory "e" [File "i" 584], File "f" 29116, File "g" 2557, File "h.lst" 62596], Directory "d" [File "d.ext" 5626152, File "d.log" 8033020, File "j" 4060174, File "k" 7214296], File "b.txt" 14848514, File "c.dat" 8504156]
  print $ getParentSize (generateFileSystem commands) "i" == 584
  print $ getParentSize (generateFileSystem commands) "g"  == 94853
  print $ getParentSize (generateFileSystem commands) "b.txt" == 48381165
  print $ getParentSize (generateFileSystem commands) "abc" == -1 


  