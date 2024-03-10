
-- task 3
isEvenIf :: Int -> String
isEvenIf x = if mod x 2 == 0 then "Yes" else "No"

isEvenGuards :: Int -> String
isEvenGuards x 
 | mod x 2 == 0 = "Yes"
 | otherwise = "No"

-- task 4
sumCubesPow :: Int -> Int -> Int
sumCubesPow x y = x ^ 3 + y ^ 3

sumCubesNoPow :: Int -> Int -> Int
sumCubesNoPow x y = x * x * x + y * y * y 

-- task 5
sqAvg :: Int -> Int -> Double
sqAvg x y = (fromIntegral $ x ^ 2 + y ^ 2) / 2


main :: IO()
main = do


    print $ isEvenIf 2 == "Yes"
    print $ isEvenIf 15452 == "Yes"
    print $ isEvenIf 321 == "No"
    print $ isEvenIf 1000 == "Yes" -- my test

    print $ isEvenGuards 2 == "Yes"
    print $ isEvenGuards 15452 == "Yes"
    print $ isEvenGuards 321 == "No"  
    print $ isEvenIf 2200 == "Yes" -- my test  



    print $ sumCubesPow 5 1 == 126
    print $ sumCubesPow 10 50 == 126000
    print $ sumCubesPow 10 2 == 1008 -- my test

    print $ sumCubesNoPow 5 1 == 126
    print $ sumCubesNoPow 10 50 == 126000
    print $ sumCubesPow 2 1 == 9 -- my test

    print $ sqAvg 5 0 == 12.5
    print $ sqAvg 10 13 == 134.5
    print $ sqAvg 21 10 == 270.5 -- my test