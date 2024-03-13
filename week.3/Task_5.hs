p :: Int -> Int 
p 1 = 1
p 2 = 5
p n = helper n ((n-1)*5) 2
 where 
    helper 2 res count = res
    helper number res count = helper (number-1) (res+count) (count+3)
main :: IO()
main = do
    print $ p 1 == 1
    print $ p 2 == 5
    print $ p 3 == 12
    print $ p 4 == 22
    print $ p 5 == 35
    print $ p 6 == 51
    print $ p 7 == 70 -- mytest
