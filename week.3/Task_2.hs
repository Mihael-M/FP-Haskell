sortN :: Int -> Int 
sortN n = (helper (removeFirstOccurrence n (biggestDigit n)) (biggestDigit n)) * 10^countZeros n
 where
    helper 0 result = result
    helper number result 
     | biggestDigit number == 0 = helper (removeFirstOccurrence number (biggestDigit number)) (result*10+(biggestDigit number))
    helper number result = helper (removeFirstOccurrence number (biggestDigit number)) (result*10+(biggestDigit number))
countZeros :: Int -> Int
countZeros 0 = 0
countZeros n 
 | mod n 10 == 0 = 1 + countZeros (div n 10)
 | otherwise = countZeros (div n 10)
biggestDigit :: Int -> Int
biggestDigit n = helper (div n 10) (mod n 10)
 where 
    helper 0 result = result
    helper number result
     | result < mod number 10 = helper (div number 10) (mod number 10)
     | otherwise = helper (div number 10) result 
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
    print $ sortN 1714 == 7411
    print $ sortN 123450 == 543210
    print $ sortN 123405 == 543210
    print $ sortN 123045 == 543210
    print $ sortN 120345 == 543210
    print $ sortN 102345 == 543210
    print $ sortN 8910 == 9810
    print $ sortN 321 == 321
    print $ sortN 29210 == 92210
    print $ sortN 1230 == 3210
    print $ sortN 55345 == 55543
    print $ sortN 14752 == 75421
    print $ sortN 329450 == 954320
    print $ sortN 9125 == 9521
    print $ sortN 1034005 == 5431000 -- mytest

