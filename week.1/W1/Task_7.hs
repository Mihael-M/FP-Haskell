isLeapYearOneLine :: Int -> Bool
isLeapYearOneLine year = mod year 400 == 0 || mod year 4 == 0  && mod year 100 /= 0


isLeapYearGuards :: Int -> Bool
isLeapYearGuards x 
 | x < 0 = error "x has to be non-negative"
 | mod x 400 == 0 = True
 | mod x 100 == 0 = False
 | mod x 4 == 0 = True
 | otherwise = False


main :: IO()
main = do


    print $ isLeapYearOneLine 2020 == True 
    print $ isLeapYearOneLine 1988 == True
    print $ isLeapYearOneLine 1600 == True
    print $ isLeapYearOneLine 2400 == True
    print $ isLeapYearOneLine 2023 == False
    print $ isLeapYearOneLine 1700 == False
    print $ isLeapYearOneLine 1800 == False
    print $ isLeapYearOneLine 2100 == False
    print $ isLeapYearOneLine 1821 == False -- my test

    print $ isLeapYearGuards 2020 == True
    print $ isLeapYearGuards 1988 == True
    print $ isLeapYearGuards 1600 == True
    print $ isLeapYearGuards 2400 == True
    print $ isLeapYearGuards 2023 == False
    print $ isLeapYearGuards 1700 == False
    print $ isLeapYearGuards 1800 == False
    print $ isLeapYearGuards 2100 == False
    print $ isLeapYearOneLine 2800 == True -- my test
