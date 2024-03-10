
rev :: Int -> Int
rev x 
 | x < 0 = 0
 | otherwise = reversing x 0
    where 
     reversing :: Int -> Int -> Int
     reversing 0 number = number
     reversing current number = reversing (div current 10) (number * 10 + mod current 10)


main :: IO()
main = do

    print $ rev 1 == 1
    print $ rev 123 == 321
    print $ rev 987654321 == 123456789
    print $ rev 345 == 543 -- my test
