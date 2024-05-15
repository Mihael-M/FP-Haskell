import Data.List
data Color = Red | Green | Blue 
 deriving (Show, Eq)
data Tree = Empty | Node Color Tree Tree 
 deriving (Show, Eq)

colorTree :: Tree
colorTree = Node Blue (Node Red (Node Green Empty Empty) (Node Red (Node Green Empty Empty) (Node Red Empty Empty))) (Node Red (Node Blue (Node Green Empty Empty) (Node Red Empty Empty)) Empty)

colorTree2 :: Tree
colorTree2 = Node Green (Node Red (Node Blue Empty Empty) (Node Blue Empty Empty)) (Node Blue (Node Green Empty Empty) (Node Blue (Node Red Empty Empty) (Node Blue Empty Empty)))

maxDepthBlueNode :: Tree -> Int
maxDepthBlueNode Empty = 0
maxDepthBlueNode (Node color left right)
 | color == Blue = 1 + max (maxDepthBlueNode left) (maxDepthBlueNode right)
 | otherwise = max (maxDepthBlueNode left) (maxDepthBlueNode right)

main::IO()
main = do
    print $ maxDepthBlueNode colorTree == 2
    print $ maxDepthBlueNode colorTree2 == 3 --mytest