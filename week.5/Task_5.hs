applyN :: (a -> a) -> Int -> (a -> a)
applyN f 0 = id 
applyN f n = f . applyN f (n - 1)  

main :: IO()
main = do
    print $ (applyN (\x -> 2 * x) 5) 2 == 64
    print $ (applyN (\x -> div x 10) 2) 100 == 1
    print $ (applyN (\x -> x + 2) 2) 100 == 104 -- mytest