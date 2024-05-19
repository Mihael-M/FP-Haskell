import Data.List
data BTree = Nil | Node Int BTree BTree
  deriving (Show, Eq)

leaves :: BTree -> [Int]
leaves Nil = []
leaves (Node x Nil Nil) = [x]
leaves (Node _ left right) = leaves left ++ leaves right


leavesAreEqual :: BTree -> BTree -> Bool
leavesAreEqual bt1 bt2 = (sort (leaves bt1)) == (sort (leaves bt2))

t1 :: BTree
t1 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 26 Nil Nil) (Node 32 Nil Nil)))

t2 :: BTree
t2 = Node 25 (Node 10 (Node 1 Nil Nil) Nil) (Node 30 (Node 32 Nil Nil) (Node 26 Nil Nil))

t3 :: BTree
t3 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 26 Nil Nil) (Node 32 Nil Nil)))

t4 :: BTree
t4 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 27 Nil Nil) (Node 32 Nil Nil)))

t5 :: BTree
t5 = Node 15 (Node 5 (Node 3 Nil Nil) (Node 7 Nil Nil)) (Node 20 (Node 18 Nil Nil) (Node 25 Nil Nil))

t6 :: BTree
t6 = Node 15 (Node 5 (Node 3 Nil Nil) (Node 7 Nil Nil)) (Node 20 (Node 18 Nil Nil) (Node 24 Nil (Node 25 Nil Nil)))

t7 :: BTree
t7 = Node 12 (Node 8 (Node 4 Nil Nil) (Node 10 Nil Nil)) (Node 16 (Node 14 Nil Nil) (Node 20 Nil (Node 22 Nil Nil)))

t8 :: BTree
t8 = Node 12 (Node 8 (Node 4 Nil Nil) (Node 10 Nil Nil)) (Node 16 (Node 14 Nil Nil) (Node 21 Nil (Node 22 Nil Nil)))


main :: IO()
main = do
    print $ leavesAreEqual t1 t2 == True
    print $ leavesAreEqual t3 t4 == False
    print $ leavesAreEqual t5 t6 == True --mytest
    print $ leavesAreEqual t7 t8 == True --mytest
