import Data.List
import Data.Char

isArithmetic :: [Int] -> Bool
isArithmetic xs = all (\i -> xs !! (i + 1) - xs !! i == xs !! (i + 2) - xs !! (i + 1)) [0..length xs - 3]

main :: IO()
main = do
    print $ isArithmetic [3] == True
    print $ isArithmetic [3, 5] == True
    print $ isArithmetic [1, 2, 3, 4, 5] == True
    print $ isArithmetic [3, 5, 7, 9, 11] == True
    print $ isArithmetic [3, 5, 8, 9, 11] == False
    print $ isArithmetic [3, 5, 9, 9, 11] == False
    print $ isArithmetic [4, 6, 8, 10, 12] == True -- mytest