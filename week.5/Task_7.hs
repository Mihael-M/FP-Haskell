switchSum :: (Num a) => (a -> a) -> (a -> a) -> Int -> a -> a
switchSum _ _ 0 = id
switchSum f g n = (\x -> (f x) + switchSum g f (n - 1) x)



main :: IO()
main = do
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 1) 2 -- == 3
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 2) 2 -- == 9
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 3) 2 -- == 16
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 4) 2 == 30
    print $ (switchSum (\x -> x + 5) (\x -> x * 3) 4) 2 == 132 -- mytest