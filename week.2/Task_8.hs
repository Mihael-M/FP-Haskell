removeD :: Int -> Int -> Int
removeD d n = rev $ removeHelper d n 0
 where
    removeHelper d 0 result = result
    removeHelper d n result
     | mod n 10 == d = removeHelper d (div n 10) result
     | otherwise = removeHelper d (div n 10) (result*10+(mod n 10))

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
    print $ removeD 1 656 == 656
    print $ removeD 5 656 == 66
    print $ removeD 6 656 == 5
    print $ removeD 0 606 == 66
    print $ removeD 0 600 == 6
    print $ removeD 6 600 == 0
    print $ removeD 2 1234 == 134
    print $ removeD 5 1255 == 12