isInteresting :: Int -> Bool
isInteresting n 
 | mod n (sumDigits n) == 0 = True
 | otherwise = False


sumDigits :: Int -> Int
sumDigits x = sumDigitsHelper 0 x
 where
 sumDigitsHelper :: Int -> Int -> Int
 sumDigitsHelper res 0 = res
 sumDigitsHelper res n = sumDigitsHelper (res+(mod n 10)) (div n 10)


main :: IO()
main = do
    print $ isInteresting 410 == True
    print $ isInteresting 212 == False
    print $ isInteresting 567 == False
    print $ isInteresting 70 == True 
    print $ isInteresting 5 == True 
    print $ isInteresting 4 == True 
    print $ isInteresting 843 == False --mytest 