type Rat a = (a, a)

normalize :: (Integral a) => Rat a -> Rat a
normalize (x, y) = (div x gcdXy, div y gcdXy)
 where
    gcdXy = gcd x y

sumRats :: (Integral a) => Rat a -> Rat a -> Rat a
sumRats (x, y) (t, n) = normalize (x * n + t * y , y * n) 

multiplyRats :: (Integral a) => Rat a -> Rat a -> Rat a
multiplyRats (x, y) (t, n) = normalize (x * t, y * n)

divideRats :: (Integral a) => Rat a -> Rat a -> Rat a
divideRats (x, y) (t, n) = normalize (x * n,y * t)

areEqual :: (Integral a) => Rat a -> Rat a -> Bool
areEqual (x, y) (t, n) = normalize (x, y) == normalize (t, n)

main :: IO()
main = do
    print $ sumRats (2, 5) (5, 2) == (29, 10)
    print $ sumRats (52, 123) (96, 14) == (6268, 861)
    print $ sumRats (2, 5) (3, 5) == (1, 1)
    print $ sumRats (4, 5) (2, 5) == (6,5) --mytest

    print $ multiplyRats (2, 5) (5, 2) == (1, 1)
    print $ multiplyRats (52, 123) (96, 14) == (832, 287)
    print $ multiplyRats (2, 5) (3, 5) == (6, 25)
    print $ multiplyRats (10, 5) (8, 5) == (16,5) --mytest

    print $ divideRats (2, 5) (5, 2) == (4, 25)
    print $ divideRats (52, 123) (96, 14) == (91, 1476)
    print $ divideRats (2, 5) (3, 5) == (2, 3)
    print $ divideRats (12, 10) (4, 5) == (3,2) --mytest

    print $ areEqual (2, 5) (5, 2) == False
    print $ areEqual (52, 123) (52 * 3, 123 * 3) == True
    print $ areEqual (2, 6) (5, 15) == True
    print $ areEqual (1, 8) (1, 8) == True --mytest