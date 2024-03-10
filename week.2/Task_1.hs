countDigitsRec :: Int -> Int
countDigitsRec n 
 | n <= 0 = 0
 | otherwise = 1 + countDigitsRec (div n 10)


countDigitsIter :: Int -> Int
countDigitsIter n 
 | n < 0 = 0
 | otherwise = countHelper 0 n 
 where 
    countHelper result 0 = result
    countHelper result leftover = countHelper (result + 1) (div leftover 10)


main :: IO()
main = do

    
    print $ countDigitsIter 12345 == 5
    print $ countDigitsIter 123 == 3
    print $ countDigitsIter 84329541 == 8 -- mytest

    print $ countDigitsRec 12345 == 5
    print $ countDigitsRec 123 == 3
    print $ countDigitsIter 843295411 == 9 -- mytest