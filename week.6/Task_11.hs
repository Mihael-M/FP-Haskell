onlyArithmetic :: [[Int]] -> [[Int]]
onlyArithmetic xss = filter (\xs -> sum xs == div (head xs + last xs) 2 * length xs) xss

main :: IO()
main = do
    print $ onlyArithmetic [[3], [1, 2, 3, 4, 5], [3, 5, 8, 9, 11]]  == [[3], [1, 2, 3, 4, 5]]
    print $ onlyArithmetic [[3], [1, 3, 5, 7, 9], [3, 5, 8, 9, 11]]  ==[[3],[1,3,5,7,9]] -- mytest