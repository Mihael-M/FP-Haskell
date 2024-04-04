type Point = (Double, Double)

line :: Point -> Point -> (Double -> Double)
line (x1,y1) (x2,y2) = (\x -> y1 + (x - x1) * (y2-y1) / (x2 - x1))

liesOn :: (Double -> Double) -> (Point -> Bool)
liesOn f = (\ (x,y) -> y == f x)

main :: IO()
main = do
    print $ (line (1,2) (2,1)) 2 == 1.0

    print $ liesOn (line (2, 3) (10, 2)) (2, (line (2, 2) (1, 4)) 1.5) == True --myTest
