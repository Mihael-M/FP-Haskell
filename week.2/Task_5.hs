sumDivs :: Int -> Int 
sumDivs 1 = 1
sumDivs n 
 | n <= 0 = 0
 | otherwise = sumHelper n 2 0
 where 
    sumHelper n devisor result
     | mod n devisor == 0 = sumHelper n (devisor+1) (result+devisor)
     | devisor >= n = result 
     | otherwise = sumHelper n (devisor+1) result 

areAmicable :: Int -> Int -> Bool
areAmicable x y 
 | sumDivs x == sumDivs y = True
 | otherwise = False

main :: IO()
main = do

    print $ areAmicable 200 300 == False
    print $ areAmicable 220 284 == True
    print $ areAmicable 284 220 == True
    print $ areAmicable 1184 1210 == True
    print $ areAmicable 2620 2924 == True
    print $ areAmicable 6232 6368 == True
    print $ areAmicable 43 68 == False -- mytest