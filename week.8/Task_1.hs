import Data.List
type Node = Int
type Graph = [(Node, [Node])]
type Path = [Node]

simplePaths :: Graph -> Int -> Node -> [Path]
simplePaths graph k n
 | k < 0     = error "Length of path must be a non-negative integer"
 | notElem n (map fst graph) = error "Node not present in graph"
 | otherwise = [path | path <- paths n k, length path == (k + 1)]
  where
    paths :: Node -> Int -> [Path]
    paths node 0 = [[node]]
    paths node steps = [node : rest | neighbor <- lookupNeighbors node, rest <- paths neighbor (steps - 1)]

    lookupNeighbors :: Node -> [Node]
    lookupNeighbors node = [neighbor | (source, targets) <- graph, source == node, neighbor <- targets]

main::IO()
main = do
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 0 1 == [[1]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 1 == [[1, 2], [1, 3]]
    print $ simplePaths [(1, [2, 3, 4]), (2, [3, 4]), (3, []), (4, [])] 1 1 == [[1,2],[1,3],[1,4]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 2 1 == [[1, 2, 3], [1, 2, 4]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 2 == [[2,3],[2,4]]
    print $ simplePaths [(1, [2, 3]), (2, [3]), (3, []), (4, [])] 1 2 == [[2,3]]
    print $ simplePaths [(1, [2, 3]), (2, [3]), (3, []), (4, [])] 2 1 == [[1,2,3]] --mytest

