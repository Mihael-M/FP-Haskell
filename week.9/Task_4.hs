type Tree = [(Int, [Int])]

t :: Tree
t = [(1,[2,3,4]),(2,[5,6]),(3,[7]),(4,[8,9]),(5,[]),(6,[10]),(7,[]),(8,[]),(9,[]),(10,[])]

findParent :: Int -> Tree -> Int
findParent _ [] = error "Node not found in tree"
findParent node ((parent, children):tree)
 | elem node children = parent
 | otherwise = findParent node tree

findSiblings :: Int -> Tree -> [Int]
findSiblings node tree = filter (/= node) (snd (head siblings))
 where
    parent = findParent node tree
    siblings = [(p, s) | (p, s) <- tree, p == parent]

findUncles :: Tree -> Int -> [Int]
findUncles tree node = findSiblings parent tree
 where
    parent = findParent node tree

main::IO()
main = do
    print $ findUncles t 5 == [3,4]
    print $ findUncles t 7 == [2,4]
    print $ findUncles t 10 == [5]
    print $ findUncles t 6 == [3,4] --mytest