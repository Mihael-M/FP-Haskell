import Data.List
type Function a = (a -> a)
type Y a = a
type Ys a = [a]

sumExpr :: (Num a, Enum a) => Function a -> Ys a -> Function a
sumExpr f ys = (\x -> sum $ zipWith (\y i -> y * f (x ^ i)) ys [1..])

main::IO()
main = do
    print $ (sumExpr (2+) [0, 1, 2, 3]) 2 == 80
    print $ (sumExpr (*0.8) [0, 1, 2, 3, 4, 5]) 10 == 4345680.0
    print $ (sumExpr (*2.5) [0, 1, 2, 3, 3, 5]) 20 == 8.25241e8 --mytest