sumPrimeDivs :: Int -> Int 
sumPrimeDivs 1 = 1
sumPrimeDivs n 
 | n <= 0 = 0
 | otherwise = sumPrimeHelper 2 0
 where 
    sumPrimeHelper devisor result
     | mod n devisor == 0 && isPrime devisor = sumPrimeHelper (devisor+1) (result+devisor)
     | devisor >= n = result 
     | otherwise = sumPrimeHelper (devisor+1) result 

isPrime :: Int -> Bool
isPrime n
 | n <= 1 = False
 | otherwise = up 2
 where
    up d
     | d * d > n = True
     | mod n d == 0 = False
     | otherwise = up (d + 1)

main :: IO()
main = do


    print $ sumPrimeDivs 0 == 0
    print $ sumPrimeDivs 6 == 5 -- 2 + 3
    print $ sumPrimeDivs 18 == 5 -- 2 + 3
    print $ sumPrimeDivs 19 == 19
    print $ sumPrimeDivs 45136 == 53
    print $ sumPrimeDivs 14 == 9 --mytest
