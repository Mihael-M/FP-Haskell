data Color = Red | Green | Blue deriving (Eq, Show)
data Tree = Empty | Node Color Tree Tree deriving Show

colorTree :: Tree
colorTree = Node Blue (Node Red (Node Green Empty Empty) Empty) (Node Red (Node Blue (Node Green Empty Empty) (Node Red Empty Empty)) Empty)

colorTree2 :: Tree
colorTree2 = Node Red (Node Green Empty Empty) (Node Blue (Node Red Empty Empty) (Node Green (Node Blue Empty Empty) Empty))

minDepthGreenNode :: Tree -> Int
minDepthGreenNode Empty = 0
minDepthGreenNode (Node Green _ _) = 0 
minDepthGreenNode (Node _ left right) = 1 + min (minDepthGreenNode left) (minDepthGreenNode right)

main :: IO()
main = do
    print $ minDepthGreenNode colorTree == 2
    print $ minDepthGreenNode colorTree2 == 1 --mytest