import Data.List
mySumRecNonPM :: [Int] -> Int
mySumRecNonPM xs
 | null xs = 0
 | otherwise = head xs + (mySumRecNonPM $ tail xs)

mySumRecPM :: [Int] -> Int
mySumRecPM [] = 0 
mySumRecPM xs = head xs + (mySumRecNonPM $ tail xs)

mySumFunc :: [Int] -> Int
mySumFunc = sum 

main :: IO()
main = do
    print $ mySumRecNonPM [] == 0
    print $ mySumRecNonPM [1, 2, 3] == 6
    print $ mySumRecNonPM [5,4,5] == 14 -- mytest

    print $ mySumRecPM [] == 0
    print $ mySumRecPM [1, 2, 3] == 6
    print $ mySumRecPM [5,4,5,8] == 22 -- mytest

    print $ mySumFunc [] == 0
    print $ mySumFunc [1, 2, 3] == 6
    print $ mySumFunc [4,5,1,3,6] == 19 -- mytest