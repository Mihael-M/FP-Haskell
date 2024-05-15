data Tree a = Nil | Node a (Tree a) (Tree a) 
 deriving (Show, Eq)

tree :: (Num a) => Tree a
tree = Node 4 (Node 1 (Node 0 Nil Nil) (Node 2 Nil (Node 3 Nil Nil))) (Node 6 (Node 5 Nil Nil) (Node 7 Nil (Node 8 Nil Nil)))


convert :: (Ord a, Num a) => Tree a -> Tree a
convert = snd . foldTree 0 where
    foldTree _ Nil = (0, Nil)
    foldTree sum (Node val left right) =
        let (rightSum, modifiedRight) = foldTree sum right
            updatedVal = val + rightSum
            (leftSum, modifiedLeft) = foldTree updatedVal left
        in (leftSum, Node updatedVal modifiedLeft modifiedRight)

main::IO()
main = do
    print $ convert tree == (Node 30 (Node 36 (Node 36 Nil Nil) (Node 35 Nil (Node 33 Nil Nil))) (Node 21 (Node 26 Nil Nil) (Node 15 Nil (Node 8 Nil Nil))))