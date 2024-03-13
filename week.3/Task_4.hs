sumDivisibleNumbers :: Int -> Int -> Int -> Int
sumDivisibleNumbers start finish k = helper (min start finish) (max start finish) 
 where 
    helper realStart realFinish
     | realStart > realFinish = 0
     | mod (sumDigits realStart) k == 0 = realStart + helper (realStart+1) realFinish 
     | otherwise = helper (realStart+1) realFinish 
sumDigits :: Int -> Int
sumDigits 0 = 0
sumDigits n = mod n 10 + (sumDigits $ div n 10)
main :: IO()
main = do
    print $ sumDivisibleNumbers 50 10 5 == 290
    print $ sumDivisibleNumbers 0 10 5 == 5
    print $ sumDivisibleNumbers 0 100 5 == 990
    print $ sumDivisibleNumbers 100 0 5 == 990
    print $ sumDivisibleNumbers 99 16 8 == 652 -- mytest