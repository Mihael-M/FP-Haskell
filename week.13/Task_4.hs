
data NTree a = NullT | Node a [(NTree a)]
 deriving (Eq, Show)

isBoring :: (Eq a) => NTree a -> Bool
isBoring NullT = True
isBoring (Node val children) = all childHasSameValue children && all isBoring children
  where
    childHasSameValue (Node v _) = v == val
    childHasSameValue NullT = True
    
t1 :: NTree Int
t1 = Node 10 [Node 10 [Node 10 [NullT], Node 8 [Node 10 [NullT]], Node 2 [NullT]], Node 10 [Node 11 [NullT], Node 10 [NullT], Node 6 [NullT]]]

t2 :: NTree Char
t2 = Node 's' [Node 's' [NullT], Node 's' [NullT], Node 's' [NullT]]

main ::IO()
main = do
    print $ isBoring t1 == False
    print $ isBoring t2 == True
