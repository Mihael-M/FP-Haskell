dotProduct :: Vector -> Vector -> Int
dotProduct (x, y, z) (t, n, k) = x * t + y * n + z * k

crossProduct :: Vector -> Vector -> Vector
crossProduct (x1, y1, z1) (x2, y2, z2) = ( y1 * z2 - z1 * y2
                                         , z1 * x2 - x1 * z2
                                         , x1 * y2 - y1 * x2
                                         )

magnitude :: Vector -> Double
magnitude (x, y, z) = sqrt (fromIntegral (x * x + y * y + z * z))

type Vector = (Int, Int, Int)

main :: IO()
main = do
    print $ dotProduct (1, 2, 3) (7, 4, 1) == 18
    print $ dotProduct (5, 2, 159) (0, -1, -2) == (-320)
    print $ dotProduct (5, 2, 220) (0, 2, -2) == -436 -- mytest

    print $ crossProduct (1, 2, 3) (7, 4, 1) == (-10, 20, -10)
    print $ crossProduct (5, 2, 159) (0, -1, -2) == (155, 10, -5)
    print $ crossProduct (5, 8, 1) (0, 2, 3) == (22,-15,10) -- mytest

    print $ magnitude (1, 2, 3) == 3.7416573867739413
    print $ magnitude (7, 4, 1) == 8.12403840463596
    print $ magnitude (-10, 20, -10) == 24.49489742783178
    print $ magnitude (5, 2, 159) == 159.0911688309568
    print $ magnitude (0, -1, -2) == 2.23606797749979
    print $ magnitude (155, 10, -5) == 155.40270267920053
    print $ magnitude (200, 15, 8) == 200.7211996775627 -- mytest