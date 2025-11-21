square :: Int -> Int
square num = num * num

myLength :: [Int] -> Int
myLength [] = 0
myLength list = 1 + myLength (tail list)

sumList :: [Int] -> Int 
sumList [] = 0
sumList list = (head list) + sumList (tail list)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1 
fib n = fib (n - 1) + fib (n - 2)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n - 1)

evens :: [Int] -> [Int]
evens list = filter even list

isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = list == reverse(list)

main::IO()
main = do
    print(myLength [1, 2, 3])
    print(sumList [1, 2, 3])
    print(fib 10)
    print(factorial 4)
    print(evens [2, 3, 4])
    print(isPalindrome [4, 2, 4])