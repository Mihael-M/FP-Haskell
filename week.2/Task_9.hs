everyOther :: Int -> Int
everyOther n 
 | n < 0 = 0
 | otherwise = everyHelper (div n 10) 0
 where 
    everyHelper 0 result = result
    everyHelper n result = everyHelper (div n 100) (result*10+mod n 10)

main :: IO()
main = do
    print $ everyOther 12 == 1
    print $ everyOther 852369 == 628
    print $ everyOther 1714 == 11
    print $ everyOther 12345 == 42
    print $ everyOther 891 == 9
    print $ everyOther 123 == 2
    print $ everyOther 2121 == 22
    print $ everyOther 4736778 == 767
    print $ everyOther 448575 == 784
    print $ everyOther 4214 == 14
    print $ everyOther 5276 == 75 -- mytest