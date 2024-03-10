removeFirstOccurrence :: Int -> Int -> Int
removeFirstOccurrence n digit = helper n 0 0
 where
    helper number leftover counter
     | mod number 10 == digit = (concatNum (div number 10) leftover)*(10^counter)
     | number == 0 = n
     | mod number 10 == 0 =  helper (div number 10) (leftover*10+(mod number 10)) (counter+1)
     | otherwise = helper (div number 10) (leftover*10+(mod number 10)) counter
concatNum :: Int -> Int -> Int
concatNum num 0 = num
concatNum num add = concatNum (num*10+(mod add 10)) (div add 10)
main :: IO()
main = do
    print $ removeFirstOccurrence 16366 5 == 16366
    print $ removeFirstOccurrence 110 1 == 10
    print $ removeFirstOccurrence 15365 5 == 1536
    print $ removeFirstOccurrence 15360 0 == 1536
    print $ removeFirstOccurrence 15300 0 == 1530
    print $ removeFirstOccurrence 15365 1 == 5365
    print $ removeFirstOccurrence 35365 3 == 3565
    print $ removeFirstOccurrence 1212 1 == 122  
    print $ removeFirstOccurrence 1212 2 == 121
    print $ removeFirstOccurrence (removeFirstOccurrence 1212 1) 1 == 22
    print $ removeFirstOccurrence 14782 7 == 1482
    