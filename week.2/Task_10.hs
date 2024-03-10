countPalindromes :: Int -> Int -> Int
countPalindromes start finish 
 | min start finish == start = helper (start+1) finish 0
 | otherwise = helper (finish+1) start 0
 where
    helper start finish result 
     | start == finish = result
     | isPalindrome start = helper (start+1) finish (result+1)
     | otherwise = helper (start+1) finish result
   
isPalindrome :: Int -> Bool
isPalindrome n
 | n < 0 = False  
 | n < 10 = True
 | mod n 10 /= mod (rev n) 10 = False
 | otherwise = isPalindrome (div n 10)
 

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
    print $ countPalindromes 5 13 == 5 -- 6 7 8 9 11
    print $ countPalindromes 13 5 == 5 -- 6 7 8 9 11
    print $ countPalindromes 25 41 == 1 -- mytest


