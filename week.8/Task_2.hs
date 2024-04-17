
type Node = (Int, Int, Int)

listLeaves :: [Node] -> [Int]
listLeaves tree = filter (\x -> notElem x parents) child
 where
    parents = map (\(x, _, _) -> x) tree
    child = concatMap (\(_, y, z) -> [y, z]) tree

main::IO()
main = do
    -- you may get slightly different results eg. [3, 4, 5] on test 1 <- not a problem
    print $ listLeaves [(1, 2, 3), (2, 4, 5)] -- == [4, 3, 5]
    print $ listLeaves [(2, 4, 5), (1, 2, 3)] == [4, 5, 3]
    print $ listLeaves [(1, 2, 3), (3, 4, 5), (5, 6, 9)] == [2, 4, 6, 9]
    print $ listLeaves [(1, 2, 3), (3, 4, 5), (4, 7, 10)] == [2,5,7,10] --mytest