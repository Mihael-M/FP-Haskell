data BTree = Nil | Node Int BTree BTree
 deriving (Show, Eq)

tree :: BTree
tree = Node 4 (Node 1 (Node 0 Nil Nil) (Node 2 Nil (Node 3 Nil Nil))) (Node 6 (Node 5 Nil Nil) (Node 7 Nil (Node 8 Nil Nil)))
tree2 :: BTree
tree2 = Node 4 (Node 1 Nil Nil) (Node 6 (Node 5 Nil Nil) (Node 7 Nil Nil))


convertHelper :: BTree -> Int -> (BTree, Int)
convertHelper Nil acc = (Nil, acc)
convertHelper (Node value left right) acc = (Node newVal newLeft newRight, finalSum)
 where
    (newRight, rightSum) = convertHelper right acc
    newVal = value + rightSum
    (newLeft, finalSum) = convertHelper left newVal


convert :: BTree -> BTree
convert tree = fst $ convertHelper tree 0


main::IO()
main = do
    print $ convert tree == (Node 30 (Node 36 (Node 36 Nil Nil) (Node 35 Nil (Node 33 Nil Nil))) (Node 21 (Node 26 Nil Nil) (Node 15 Nil (Node 8 Nil Nil))))
    print $ convert tree2 == Node 22 (Node 23 Nil Nil) (Node 13 (Node 18 Nil Nil) (Node 7 Nil Nil)) --mytest