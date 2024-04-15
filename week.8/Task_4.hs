import Data.List
data Shape a = Circle a | Rectangle a a | Triangle a a a | Cylinder a a
 deriving (Show, Eq, Read, Ord)

area :: Floating a => Shape a -> a
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h
area (Cylinder r h) = 2 * pi * r * h + 2 * pi * r * r
area (Triangle a b c) = sqrt (s * (s - a) * (s - b) * (s - c))
 where 
     s = (a + b + c) / 2  

getAreas :: Floating a => [Shape a] -> [a]
getAreas shapes = map area shapes

maxArea :: (Ord a, Floating a) => [Shape a] -> Shape a
maxArea shapes = foldl1 (\acc x -> if area x > area acc then x else acc) shapes

main::IO()
main = do
    print $ getAreas [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == [78.53981633974483,11.25,113.30000000000001,9.127927385194024,6283.185307179587]
    print $ maxArea [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == Cylinder 20.0 30.0
    print $ maxArea [Circle 10, Rectangle 2 4.5, Rectangle 5 20.9, Triangle 5.3 2.4 4.89, Cylinder 10 30] == Cylinder 10.0 30.0 --mytest