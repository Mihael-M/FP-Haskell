data BTree = Empty | Node Int BTree BTree
 deriving (Show, Eq)

levelSum :: BTree -> Int -> Int
levelSum Empty _ = 0
levelSum (Node value left right) 0 = value
levelSum (Node _ left right) k = levelSum left (k - 1) + levelSum right (k - 1)

cone :: BTree -> Bool
cone tree = checkCone tree 0 (-1)

checkCone :: BTree -> Int -> Int -> Bool
checkCone tree level prevSum
 | currentSum == 0 = True  
 | prevSum >= 0 && currentSum <= prevSum = False
 | otherwise = checkCone tree (level + 1) currentSum
 where 
    currentSum = levelSum tree level

numberBTree = Node 10 (Node 5 (Node 1 Empty Empty) (Node 9 Empty Empty)) (Node 6 (Node 8 Empty Empty) (Node 7 Empty Empty))

anotherBTree = Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 (Node 6 Empty Empty) Empty)
main::IO()
main = do
    print $ levelSum numberBTree 1 == 11 -- (5 + 6)
    print $ levelSum anotherBTree 1 == 5--mytest
    print $ cone numberBTree == True
    print $ cone anotherBTree == True --mytest
