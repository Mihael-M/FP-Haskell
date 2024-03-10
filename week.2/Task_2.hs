sumDigitsIter :: Int -> Int
sumDigitsIter n
 | n < 0 = 0 
 | otherwise = sumHelper n 0
 where 
    sumHelper 0 sum = sum
    sumHelper num sum = sumHelper (div num 10) (sum + mod num 10)




main :: IO()
main = do

    print $ sumDigitsIter 12345 == 15
    print $ sumDigitsIter 123 == 6
    print $ sumDigitsIter 3411 == 9 -- mytest
