import Data.List
data BTree a = NullT | Node a (BTree a) (BTree a)
 deriving (Show)

maxSumSubT :: (Ord a, Num a) => BTree a -> a
maxSumSubT NullT = 0
maxSumSubT (Node val left right) = 
    let 
        leftSum = maxSumSubT left
        rightSum = maxSumSubT right
        subtreeSum = val + leftSum + rightSum
    in 
        max subtreeSum (max leftSum rightSum)

t1 :: (Num a) => BTree a 
t1 = Node 3 (Node 0 NullT NullT) (Node 2 (Node 0 NullT NullT) NullT )

t2 :: (Num a) => BTree a 
t2 = Node (-3) (Node 0 NullT NullT) (Node 2 (Node 0 NullT NullT ) NullT )


main ::IO()
main = do
    print $ maxSumSubT t1 == 5
    print $ maxSumSubT t2 == 2
