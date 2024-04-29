import Data.List
data Tree a = Nil | Node (a, a) (Tree a) (Tree a)
 deriving (Show, Eq)

t1 :: Tree Int
t1 = Node (3, 10) (Node (5, 8) (Node (6, 7) Nil Nil) (Node (4, 9) Nil Nil)) (Node (2, 12) Nil (Node (1, 15) Nil Nil))

t2 :: Tree Int
t2 = Node (3, 10) (Node (5, 8) (Node (6, 7) Nil Nil) (Node (7, 9) Nil Nil)) (Node (2, 12) Nil (Node (1, 15) Nil Nil))

ordered :: (Ord a) => Tree a -> Bool
ordered Nil = True
ordered tree = sort (traverseDFS tree) == reverse (traverseDFS tree)

traverseDFS :: (Ord a) => Tree a -> [(a, a)]
traverseDFS Nil = []
traverseDFS (Node value left right) = traverseDFS left ++ [value] ++ traverseDFS right

main :: IO()
main = do
    print $ ordered t1 == True
    print $ ordered t2 == False