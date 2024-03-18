import Data.List

isPresentRecNonPM :: Int -> [Int] -> Bool
isPresentRecNonPM n xs = (not $ null xs) && (n == head xs || isPresentRecNonPM n (tail xs))

isPresentRecPM :: Int -> [Int] -> Bool
isPresentRecPM  _ [] = False
isPresentRecPM n (x:xs) = n == x || isPresentRecPM n xs 

isPresentFunc :: Int -> [Int] -> Bool
isPresentFunc = elem 
main :: IO()
main = do
    print $ isPresentRecNonPM 0 [] == False
    print $ isPresentRecNonPM 0 [1, 2, 3] == False
    print $ isPresentRecNonPM 0 [0, -1, 2] == True
    print $ isPresentRecNonPM 2 [5, 2, 8, 4] == True -- mytest


    print $ isPresentRecPM 0 [] == False
    print $ isPresentRecPM 0 [1, 2, 3] == False
    print $ isPresentRecPM 0 [0, -1, 2] == True
    print $ isPresentRecPM 5 [5, 4, 99] == True -- mytest

    print $ isPresentFunc 0 [] == False
    print $ isPresentFunc 0 [1, 2, 3] == False
    print $ isPresentFunc 0 [0, -1, 2] == True
    print $ isPresentFunc 3 [1, 2, 3, 4] == True -- mytest