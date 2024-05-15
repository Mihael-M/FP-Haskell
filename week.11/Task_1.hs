import Data.List

myPoly :: [Double] -> (Double -> Int -> Double)
myPoly xs = (\x y -> product (take y [(x - xi) | xi <- xs]))

main :: IO ()
main = do
    print $ (myPoly [2.7, 3.0 ..] ) 2.2 3 == -0.4399999999999998
    print $ (myPoly [1.0, 2.0 ..] ) 1.2 2 == -0.15999999999999998 -- mytest
