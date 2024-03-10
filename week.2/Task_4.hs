countOccurrences :: Int -> Int -> Int
countOccurrences 0 0 = 1
countOccurrences num wanted = countHelper num wanted 0
 where 
    countHelper 0 wanted count = count
    countHelper num wanted count 
     | mod num 10 == wanted = countHelper (div num 10) wanted (count+1)
     | otherwise = countHelper (div num 10) wanted count
main :: IO()
main = do

    print $ countOccurrences 121 1 == 2
    print $ countOccurrences 222 1 == 0
    print $ countOccurrences 100 0 == 2
    print $ countOccurrences 0 0 == 1
    print $ countOccurrences 345252 2 == 2 -- mytest
