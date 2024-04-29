import Data.List
data NaryTree a = Nil | Node a [NaryTree a] 
 deriving Show

t1 :: NaryTree Int
t1 = Node 1 [Node 3 [], Node 5 [], Node 7 [], Node 9 []]

t2 :: NaryTree Int
t2 = Node 7 [Node 9 [Node 5 [], Node 2 []]]

t3 :: NaryTree Int
t3 = Node 4 [Node 7 [Node 11 [], Node 3 []], Node 6 [], Node 10 [Node 8 []]]

isGraceful :: (Integral a) => NaryTree a -> Bool
isGraceful Nil = True 
isGraceful (Node _ children) = all even differences && all isGraceful children
 where
    parentVal = head [x | Node x _ <- children] 
    childVals = [x | Node x _ <- children] 
    differences = map (\x -> abs (x - parentVal)) childVals

main :: IO()
main = do
    print $ isGraceful t1 == True
    print $ isGraceful t2 == False
    print $ isGraceful t3 == False --mytest